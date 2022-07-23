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
    '>': 'gt',
    '<': 'lt',
    '>=': 'lte',
    '<=': 'gte',
    '==': 'eq',
    '!=': 'neq',
}

const dataTypeTag = '__$tag'
const dataTypeValues = '__$values'

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
        return ast.items.map(item => this.genItem(item)).join('\n')
    }

    public genStmt(stmt: Stmt): string {
        switch (stmt.tag) {
        case 'Expr': return this.genExpr(stmt.expr)
        case 'Item': return this.genItem(stmt.item)
        }
    }

    public genExpr(expr: Expr): string {
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
        case 'Cons': return `${expr.cons}${expr.args.map(arg => `(${this.genExpr(arg)})`)}`
        }
    }

    public genBlock(block: Block): string {
        const returnExpr = block.stmts.pop()

        return `
(function() {
${block.stmts.map(stmt => `${this.indentStr()}${stmt}`).join(';\n')};
${returnExpr ? `\n${this.indentStr()}return ${returnExpr}` : ''}
${this.dedent()}}())
`.trim()
    }

    public genItem(item: Item): string {
        switch (item.tag) {
        case 'Term': {
            return `const ${item.name} = ${
                item.params.reduce((params, p) => `${params}(${p}) => `, '')
            } ${this.genExpr(item.body)};`
        }
        case 'Data': {
            // data A = C1 | C2
            // const C1 = {__$tag: 'C1', __$value: []}
            // const C2 = {__$tag: 'C2', __$values: []}

            // data Opt a = Some a | None
            // const Some = (_1) => {__$tag: 'Some', __$values: [_1]}
            // const None = {__$tag: 'None'}
            return item.cons.map(con =>
                `const ${
                    con.name
                } = ${
                    con.types.reduce((tys, _, i) => `${tys}(_${i}) => `, '')
                } {${
                    dataTypeTag
                }: '${
                    con.name
                }', ${
                    dataTypeValues
                }: [${
                    con.types.map((_, i) => `_${i}`)
                }]}`)
                .join('\n')
        }
        case 'Ty': return ''
        }
    }
}
