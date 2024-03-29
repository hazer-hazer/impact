import { Expr } from './ast'

export class ParserCtx {
    private indentStack: string[] = []
    private indent = ''

    public curIndent(): string {
        return this.indent
    }

    public pushIndent(spaces: string) {
        this.indentStack.push(spaces)
        this.indent = spaces
    }

    public popIndent(): boolean {
        this.indentStack.pop()
        return true // Return true for convenient use as and predicate
    }

    public isIndent(spaces: string): boolean {
        return spaces.length > this.curIndent().length
    }

    public isSameIndent(spaces: string): boolean {
        return spaces === this.indent
    }

    public reset(): this {
        this.indentStack = []
        this.indent = ''
        return this
    }

    // LA = left-associative
    public makeInfix(lhs: Expr, ops: [string, Expr][]): Expr {
        return ops.reduce((infix, [op, rhs]) => ({
            tag: 'App',
            lhs: {
                tag: 'App',
                lhs: {
                    tag: 'Var',
                    name: op,
                },
                arg: infix,
            },
            arg: rhs,
        }), lhs)
    }

    public makeApp(lhs: Expr, args: Expr[]): Expr {
        return args.reduce((lhs, arg) => ({
            tag: 'App',
            lhs,
            arg,
        }), lhs)
    }
}
