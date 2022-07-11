import { assert } from "console"
import { AST, Expr, Item, Stmt } from "./ast"

export type Options = {
    indentSize: number
}

const defaultOptions: Options = {
    indentSize: 4
}

export class JSGen {
    options: Options

    indentLevel: number = 0

    constructor(options: Options = defaultOptions) {
        this.options = { ...defaultOptions, ...options }
    }

    private reset(): this {
        this.indentLevel = 0
        return this
    }

    private indent(): string {
        this.indentLevel++
        return this.indent_str()
    }

    private dedent(): string {
        this.indentLevel--
        assert(this.indentLevel >= 0)
        return this.indent_str()
    }

    private indent_str(): string {
        return ' '.repeat(this.options.indentSize).repeat(this.indentLevel)
    }

    public gen(ast: AST): string {
        return ast.stmts.map(this.genStmt).join('\n')
    }

    private genStmt(stmt: Stmt): string {
        switch (stmt.tag) {
            case 'Expr': return this.genExpr(stmt.expr)
            case 'Item': return this.genItem(stmt.item)
        }
    }

    private genExpr(expr: Expr): string {
        switch (expr.tag) {
            case 'Abs': {
                return `${expr.param} => ${this.genExpr(expr.body)}`
            }
            case 'Var': return expr.name
            case 'Lit': {
                switch (expr.kind.tag) {
                    case 'Unit': return '()'
                    case 'Bool': return expr.kind.val.toString()
                    case 'Int': return expr.kind.val.toString()
                    case 'String': return expr.kind.val
                }
            }
            case 'Infix': return `${this.genExpr(expr.lhs)} ${expr.op} ${this.genExpr(expr.rhs)}`
            case 'Block': {
                this.indent();

                let s = `${expr
                    .block
                    .stmts
                    .map(stmt => `${this.indent_str()}${this.genStmt(stmt)}`)
                    .join('\n')
                    }`

                let e = `${this.indent_str()}${this.genExpr(expr.block.expr)}`

                this.dedent();

                return `${expr.block.stmts.length ? '\n' : ''}${s}\n${e}`
            }
            case 'App': return `${this.genExpr(expr.lhs)} ${this.genExpr(expr.arg)}`
            case 'Let': return `let ${this.genExpr(expr.body)}`
            case 'Anno': return ''
        }
    }

    private genItem(item: Item): string {
        switch (item.tag) {
            case 'Term': {
                if (item.params.length) {
                    return `function ${item.name}(${item.params.join(', ')}) {\n${this.indent()}return ${item.body}\n${this.dedent()}}`.trim()
                } else {
                    return `const ${item.name} = ${this.genExpr(item.body)};`.trim()
                }
            }
            case 'Ty': return `type ${item.name} = ${item.ty}`
        }
    }

    private infix(lhs: string, op: string, rhs: string): string {
        return `${lhs} ${op} ${rhs}`
    }

    private block(stmts: string[]): string {
        let return_expr = stmts.pop();

        this.indent();
        return `
(function() {
${stmts.map(stmt => `${this.indent_str()}${stmt}`).join(';\n')};
${return_expr ? `\n${this.indent_str()}return ${return_expr}` : ''}
${this.dedent()}}())
`.trim()
    }

    call(lhs: string, arg: string): string {
        return `${lhs}(${arg})`
    }
}