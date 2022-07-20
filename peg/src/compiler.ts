import chalk from 'chalk'
import { readFileSync } from 'fs'
import { generate, Parser, SourceText } from 'peggy'
import { Context, createContext, runInContext } from 'vm'
import { AST, Expr, PP, Stmt, Ty as AstTy } from './ast'
import { JSGen } from './js-gen'
import { ParserCtx } from './parser-ctx'
import { prelude } from './prelude'
import { conv, Ctx, InferErr, ppTy, Ty } from './typeck'
import { Result } from './types'

export type Options = {
    printJS?: boolean
    printAst?: boolean
    printSource?: boolean
}

const defaultOptions: Options = {
    printJS: false,
    printSource: false,
    printAst: false,
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
            allowedStartRules: ['line', 'ty', 'expr'],
        })

        this.ctx = createContext()
 
        this.jsGen = new JSGen()
        this.parserCtx = new ParserCtx()
        this.tyCtx = new Ctx()

        this.options = { ...defaultOptions, ...options }

        this.initContext()
    }

    private initContext() {
        this.tyCtx = new Ctx()
        const jsPrelude: Record<string, any> = {}
        for (const [name, decl] of Object.entries(prelude)) {
            const source = `prelude/${name}`
            const ty = this.parse<AstTy>(source, decl[0], 'ty').unwrap('Failed to parse some prelude declaration')
            this.tyCtx.addInPlace({
                tag: 'TypedTerm',
                name,
                ty: conv(ty),
            })
            jsPrelude[JSGen.qualifyName(name)] = decl[1]
        }
        this.ctx = createContext(jsPrelude)
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

            const ty = this
                .typeck({
                    tag: 'Expr',
                    expr,
                })
                .mapErr(printErr)
                .unwrapWith(null)

            console.log(ppTy(ty))                
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

    typeck(stmt: Stmt): Result<Ty, InferErr> {
        try {
            const [ty, ctx] = this.tyCtx.synth(stmt)
            this.tyCtx = ctx
            return Result.Ok(ty)
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
                const pp = new PP()
    
                console.log(`AST:\n${pp.pp(ast)}`)
            }

            const ty = this
                .typeck(ast.stmt)
                .mapErr(printErr)
                .unwrapWith(null)

            console.log(chalk.magenta(`:${ppTy(this.tyCtx.apply(ty))}`))

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
