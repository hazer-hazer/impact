import { readFileSync } from "fs"
import { generate, GrammarError, Parser, parser } from "peggy"
import { Context, createContext, runInContext } from "vm"
import { JSGen } from "./js-gen"
import { ParserCtx } from "./parser-ctx"
import { prelude } from "./prelude"

export type Options = {
    printJS?: boolean
    printSource?: boolean
}

const defaultOptions: Options = {
    printJS: false,
    printSource: false
}

export class Compiler {
    private grammar: string
    private parser: Parser
    private ctx: Context
    private options: Options
    private jsGen: JSGen
    private parserCtx: ParserCtx

    constructor(options: Options) {
        this.grammar = readFileSync('./peggy-js.pegjs', 'utf-8')
        this.parser = generate(this.grammar, {
            allowedStartRules: ['program'],
        })

        this.ctx = createContext({ ...prelude })

        this.jsGen = new JSGen()
        this.parserCtx = new ParserCtx()

        this.options = { ...defaultOptions, ...options }
    }

    private resetContext() {
        this.ctx = createContext({ ...prelude })
    }

    runCommand(command: string, args: string[]): any | undefined {
        switch (command) {
            case 'run': {
                return this.exec(`${args[0]}(${args.slice(1).join(', ')})`)
            }
            case 'reset': {
                this.resetContext()
                process.stdout.write('\u001B[2J\u001B[0;0f')
                return;
            }
            case 'ctx': {
                console.log(this.ctx);
                return;
            }
            default: {
                throw new Error(`Unknown command ${command}`)
            }
        }
    }

    exec(code: string): any | undefined {
        return runInContext(code, this.ctx);
    }

    run(code: string): any | undefined {
        try {
            if (this.options.printSource) {
                console.log(`Source:\n\`${code}\``);
            }

            const transpiled = this.parser.parse(code, {
                jsGen: this.jsGen,
                parserCtx: this.parserCtx
            });

            if (this.options.printJS) {
                console.log(`JS code: \`${transpiled}\``);
            }

            return this.exec(transpiled);
        } catch (e: any) {
            if (e instanceof parser.SyntaxError || e instanceof GrammarError) {
                throw new Error(e.format([
                    { source: this.grammar, text: code },
                ]))
            } else {
                throw e
            }
        }
    }
}
