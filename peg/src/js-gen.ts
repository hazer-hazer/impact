import { assert } from 'console'
import { AST, Block, Expr, Item, Stmt } from './ast'

export type Options = {
    indentSize: number
}

const defaultOptions: Options = {
    indentSize: 4,
}

const builtNames: Record<string, string> = {
    '+': 'plus',
    '-': 'minus',
    '*': 'pow',
    '/': 'div',
    '%': 'mod',
}

export class JSGen {
    options: Options

    indentLevel = 0

    constructor(options: Options = defaultOptions) {
        this.options = { ...defaultOptions, ...options }
    }

    public static qualifyName(name: string): string {
        if (name in builtNames) {
            return `__$${builtNames[name]}`
        }
        return name
    }

    public reset(): this {
        this.indentLevel = 0
        return this
    }

    private indent(): string {
        this.indentLevel++
        return this.indentStr()
    }

    private dedent(): string {
        this.indentLevel--
        assert(this.indentLevel >= 0)
        return this.indentStr()
    }

    private indentStr(): string {
        return ' '.repeat(this.options.indentSize).repeat(this.indentLevel)
    }

    public gen(ast: AST): string {
        return this.genStmt(ast.stmt)
        // return ast.stmts.map(stmt => this.genStmt(stmt)).join('\n')
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
            return `(${expr.param} => ${this.genExpr(expr.body)})`
        }
        case 'Var': return JSGen.qualifyName(expr.name)
        case 'Lit': {
            switch (expr.kind.tag) {
            case 'Unit': return '()'
            case 'Bool': return expr.kind.val.toString()
            case 'Int': return expr.kind.val.toString()
            case 'String': return expr.kind.val
            }
            break
        }
        case 'Block': {
            return this.genBlock(expr.block)
        }
        case 'App': return `${this.genExpr(expr.lhs)}(${this.genExpr(expr.arg)})`
        case 'Let': return `let ${this.genBlock(expr.body)}`
        case 'Anno': return this.genExpr(expr.expr)
        case 'If': return `(${this.genExpr(expr.cond)} ? ${this.genExpr(expr.then)} : ${this.genExpr(expr.else)})`
        }
    }

    private genBlock(block: Block): string {
        const returnExpr = block.stmts.pop()

        return `
(function() {
${block.stmts.map(stmt => `${this.indentStr()}${stmt}`).join(';\n')};
${returnExpr ? `\n${this.indentStr()}return ${returnExpr}` : ''}
${this.dedent()}}())
`.trim()
    }

    private genItem(item: Item): string {
        switch (item.tag) {
        case 'Term': {
            if (item.params.length) {
                return `function ${item.name}(${item.params.join(', ')}) {\n${this.indent()}return ${this.genExpr(item.body)}\n${this.dedent()}}`.trim()
            } else {
                return `const ${item.name} = ${this.genExpr(item.body)};`.trim()
            }
        }
        case 'Ty': return `type ${item.name} = ${item.ty}`
        }
    }
}
