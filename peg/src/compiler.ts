import chalk from 'chalk'
import { assert, table } from 'console'
import { readFileSync } from 'fs'
import { generate, Parser, SourceText } from 'peggy'
import { Context, createContext, runInContext } from 'vm'
import { AST, Expr, Item, pp, PP, ppExpr, Stmt, Ty as AstTy } from './ast'
import { JSGen } from './js-gen'
import { ParserCtx } from './parser-ctx'
import { firstClassPrelude, prelude } from './prelude'
import { conv, Ctx, InferErr, ppTy, Ty } from './typeck'
import { Result } from './types'

export type Options = {
    printJS?: boolean
    printAst?: boolean
    printSource?: boolean
    printPrelude?: boolean
}

const defaultOptions: Options = {
    printJS: false,
    printSource: false,
    printAst: false,
    printPrelude: false,
}

interface PegSyntaxError extends Error {
    format(sources: SourceText[]): string;
}

const printSyntaxError = (err: PegSyntaxError, source: string, code: string) => {
    console.log(chalk.red(err.format([
        { source, text: code },
    ])))
}

const printErr = <E extends Error>(err: E) => {
    console.log(chalk.red(err.message))
}

const isSyntaxError = (e: any): e is PegSyntaxError => 'format' in e

export class Compiler {
    private grammar: string
    private parser: Parser
    private ctx: Context
    private options: Options
    private jsGen: JSGen
    private parserCtx: ParserCtx
    private tyCtx: Ctx

    constructor(options: Options) {
        this.grammar = readFileSync('./peggy-js.pegjs', 'utf-8')
        this.parser = generate(this.grammar, {
            allowedStartRules: ['line', 'ty', 'expr', 'item'],
        })

        this.ctx = createContext()

        this.jsGen = new JSGen()
        this.parserCtx = new ParserCtx()
        this.tyCtx = new Ctx()

        this.options = { ...defaultOptions, ...options }

        this.initContext()
    }

    private initContext() {
        const jsPrelude: Record<string, any> = {}

        // First-class prelude
        const source = 'first-class-prelude'
        const ast = this
            .parse<AST>(source, firstClassPrelude, 'ast')
            .mapErr(err => printSyntaxError(err, source, firstClassPrelude))
            .unwrap(`Failed to parse first-class prelude`)

        const [_, ctx] = this.typeck(ast.stmt).unwrap(`Type check failed on first-class prelude`)

        this.exec(this.jsGen.gen(ast))

        let tyCtx = ctx

        // JS prelude
        for (const [name, decl] of Object.entries(prelude)) {
            const source = `prelude/${name}`

            switch (decl.tag) {
                case 'func':
                case 'val': {
                    const ty = this.parse<AstTy>(source, decl.ty, 'ty').unwrap(`Failed to parse type of ${decl.tag} '${name}' from prelude`)
                    tyCtx.addInPlace({
                        tag: 'TypedTerm',
                        name,
                        ty: conv(ty),
                    })

                    let jsCode: any
                    if (decl.tag === 'func') {
                        jsCode = decl.func
                    } else if (decl.tag === 'val') {
                        jsCode = decl.val
                    }

                    jsPrelude[JSGen.qualifyName(name)] = jsCode
                    break
                }
            }
        }

        this.tyCtx = tyCtx
        this.ctx = createContext(jsPrelude)

        if (this.options.printPrelude) {
            console.log(JSON.stringify(jsPrelude, null, 2));
        }
    }

    runCommand(command: string, args: string[]): unknown | undefined {
        switch (command) {
            case 'run': {
                return this.exec(`${args[0]}(${args.slice(1).join(', ')})`)
            }
            case 'reset': {
                this.initContext()
                process.stdout.write('\u001B[2J\u001B[0;0f')
                return
            }
            case 'ctx': {
                console.log(this.ctx)
                console.log(this.tyCtx.pp())
                return
            }
            case 't': {
                if (!args[0]) {
                    throw new Error('Expected type name as an argument')
                }
                const source = '[:t command]'
                const expr = this
                    .parse<Expr>(source, args[0], 'expr')
                    .mapErr(err => printSyntaxError(err, source, args[0]))
                    .unwrapWith(null)

                // TODO: Set this to resulting context?
                const [ty, _] = this
                    .typeck({
                        tag: 'Expr',
                        expr,
                    })
                    .mapErr(printErr)
                    .unwrapWith(null)

                console.log(chalk.magenta(`${ppExpr(expr)}: ${ppTy(ty)}`))
                return
            }
            default: {
                throw new Error(`Unknown command ${command}`)
            }
        }
    }

    exec(code: string): unknown | undefined {
        return runInContext(code, this.ctx)
    }

    parse<T>(source: string, code: string, startRule: string): Result<T, PegSyntaxError> {
        try {
            return Result.Ok(this.parser.parse(code, {
                parserCtx: this.parserCtx,
                startRule,
                grammarSource: source,
            }))
        } catch (e) {
            // Idk why the heck peggy does not export its SyntaxError so I could use instanceof 🙄
            if (isSyntaxError(e)) {
                return Result.Err(e)
            }
            throw e
        }
    }

    typeck(stmt: Stmt, ctx = this.tyCtx): Result<[Ty, Ctx], InferErr> {
        try {
            const [ty, resultCtx] = ctx.synth(stmt)
            return Result.Ok([resultCtx.apply(ty), resultCtx])
        } catch (e) {
            if (e instanceof InferErr) {
                return Result.Err(e)
            }
            throw e
        }
    }

    run(code: string): unknown | undefined {
        try {
            if (this.options.printSource) {
                console.log(`Source:\n\`${code}\``)
            }

            const source = 'REPL'

            const ast = this
                .parse<AST>(source, code, 'line')
                .mapErr(err => printSyntaxError(err, source, code))
                .unwrapWith(null)

            if (this.options.printAst) {
                console.log(`AST:\n${pp(ast)}`)
            }

            const [ty, ctx] = this
                .typeck(ast.stmt)
                .mapErr(printErr)
                .unwrapWith(null)

            this.tyCtx = ctx

            console.log(chalk.magenta(`${pp(ast)}: ${ppTy(ty)}`))

            const js = this.jsGen.gen(ast)

            if (this.options.printJS) {
                console.log(`JS Code:\n${js}`)
            }

            return this.exec(js)
        } catch (e) {
            if (e instanceof InferErr) {
                console.log(chalk.red(e.message))
                return
            }

            if (e instanceof Error) {
                console.log(e.message, e.constructor.name)
            }

            if (e) {
                throw e
            }
        }
    }
}
