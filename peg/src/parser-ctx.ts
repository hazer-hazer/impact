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

    public makeInfix(lhs: Expr, op: string, rhs: Expr): Expr {
        return {
            tag: 'App',
            lhs: {
                tag: 'App',
                lhs: {
                    tag: 'Var',
                    name: op,
                },
                arg: lhs,
            },
            arg: rhs,
        }
    }
}
