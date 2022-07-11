export type Ty = {
    tag: 'Lit'
    kind: {
        tag: 'Bool'
    } | {
        tag: 'Int'
    } | {
        tag: 'String'
    }
} | {
    tag: 'Unit'
} | {
    tag: 'Var'
    name: string
} | {
    tag: 'Func'
    param: Ty
    ret: Ty
}

export type Block = {
    stmts: Stmt[],
    expr: Expr,
}

export type Expr = {
    tag: 'Var'
    name: string
} | {
    tag: 'Lit',
    kind: {
        tag: 'Unit'
    } | {
        tag: 'Bool'
        val: boolean
    } | {
        tag: 'Int'
        val: number
    } | {
        tag: 'String'
        val: string
    }
} | {
    tag: 'Abs'
    param: string
    body: Expr
} | {
    tag: 'App'
    lhs: Expr
    arg: Expr
} | {
    tag: 'Let'
    body: Expr
} | {
    tag: 'Anno'
    expr: Expr
    ty: Ty
} | {
    tag: 'Infix'
    lhs: Expr
    op: string
    rhs: Expr
} | {
    tag: 'Block',
    block: Block,
}

export type Item = {
    tag: 'Ty'
    name: string
    ty: Ty
} | {
    tag: 'Term'
    name: string
    params: string[]
    body: Expr
}

export type Stmt = {
    tag: 'Expr'
    expr: Expr
} | {
    tag: 'Item'
    item: Item
}

export type AST = {
    stmts: Stmt[]
}

// PP //
export class PP {
    constructor(private indent_level: number = 0) { }

    private indent() {
        this.indent_level++
    }

    private dedent() {
        this.indent_level--
    }

    private indent_str() {
        return '  '.repeat(this.indent_level)
    }

    public pp(ast: AST): string {
        return ast.stmts.map(stmt => this.ppStmt(stmt)).join('\n')
    }

    private ppExpr(expr: Expr): string {
        switch (expr.tag) {
            case 'Abs': return `\\${expr.param} -> ${this.ppExpr(expr.body)}`
            case 'Var': return expr.name
            case 'Lit': {
                switch (expr.kind.tag) {
                    case 'Unit': return '()'
                    case 'Bool': return expr.kind.val.toString()
                    case 'Int': return expr.kind.val.toString()
                    case 'String': return expr.kind.val
                }
            }
            case 'Anno': return `${this.ppExpr(expr.expr)}: ${this.ppTy(expr.ty)}`
            case 'Infix': return `${this.ppExpr(expr.lhs)} ${expr.op} ${this.ppExpr(expr.rhs)}`
            case 'Block': {
                this.indent();

                let s = `${expr
                    .block
                    .stmts
                    .map(stmt => `${this.indent_str()}${this.ppStmt(stmt)}`)
                    .join('\n')
                    }`

                let e = `${this.indent_str()}${this.ppExpr(expr.block.expr)}`

                this.dedent();

                return `${expr.block.stmts.length ? '\n' : ''}${s}\n${e}`
            }
            case 'App': return `${this.ppExpr(expr.lhs)} ${this.ppExpr(expr.arg)}`
            case 'Let': return `let ${this.ppExpr(expr.body)}`
        }
    }

    private ppStmt(stmt: Stmt): string {
        switch (stmt.tag) {
            case 'Expr': return this.ppExpr(stmt.expr)
            case 'Item': return this.ppItem(stmt.item)
        }
    }

    private ppItem(item: Item): string {
        switch (item.tag) {
            case 'Term': {
                return `${item.name} ${item.params.length ? ' ' : ''}${item.params.join(' ')}= ${this.ppExpr(item.body)}`
            }
            case 'Ty': return `type ${item.name} = ${item.ty}`
        }
    }

    private ppTy(ty: Ty): string {
        switch (ty.tag) {
            case 'Lit': {
                switch (ty.kind.tag) {
                    case 'Bool': return 'Bool'
                    case 'Int': return 'Int'
                    case 'String': return 'String'
                }
            }
            case 'Unit': return '()'
            case 'Func': return `${ty.param} -> ${ty.ret}`
            case 'Var': return ty.name
        }
    }
}
