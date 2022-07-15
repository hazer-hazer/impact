import chalk from 'chalk'
import { readFileSync } from 'fs'
import { generate, GrammarError, Parser, parser } from 'peggy'
import { Context, createContext, runInContext } from 'vm'
import { AST, PP } from './ast'
import { JSGen } from './js-gen'
import { ParserCtx } from './parser-ctx'
import { prelude } from './prelude'
import { Ctx, InferErr, ppTy } from './typeck'

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
            allowedStartRules: ['line'],
        })

        this.ctx = createContext({ ...prelude })

        this.jsGen = new JSGen()
        this.parserCtx = new ParserCtx()
        this.tyCtx = new Ctx()

        this.options = { ...defaultOptions, ...options }
    }

    private resetContext() {
        this.ctx = createContext({ ...prelude })
    }

    runCommand(command: string, args: string[]): unknown | undefined {
        switch (command) {
        case 'run': {
            return this.exec(`${args[0]}(${args.slice(1).join(', ')})`)
        }
        case 'reset': {
            this.resetContext()
            process.stdout.write('\u001B[2J\u001B[0;0f')
            return
        }
        case 'ctx': {
            console.log(this.ctx)
            console.log(this.tyCtx.pp())
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

    run(code: string): unknown | undefined {
        try {
            if (this.options.printSource) {
                console.log(`Source:\n\`${code}\``)
            }

            const ast: AST = this.parser.parse(code, {
                parserCtx: this.parserCtx,
            })

            if (this.options.printAst) {
                const pp = new PP()
    
                console.log(`AST:\n${pp.pp(ast)}`)
            }

            const [ty, ctx] = this.tyCtx.synth(ast.stmt)
            this.tyCtx = ctx

            console.log(chalk.magenta(`:${ppTy(this.tyCtx.apply(ty))}`))

            const js = this.jsGen.gen(ast)

            if (this.options.printJS) {
                console.log(`JS Code:\n${js}`)
            }

            return this.exec(js)
        } catch (e: unknown) {
            if (e instanceof parser.SyntaxError || e instanceof GrammarError) {
                console.log(chalk.red(e.format([
                    { source: this.grammar, text: code },
                ])))
                return
            }

            if (e instanceof InferErr) {
                console.log(chalk.red(e.message))
                return
            }

            throw e
        }
    }
}
