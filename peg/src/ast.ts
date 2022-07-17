export type Ty = {
    tag: 'Lit'
    kind: {
        tag: 'Unit'
    } | {
        tag: 'Bool'
    } | {
        tag: 'Int'
    } | {
        tag: 'String'
    }
} | {
    tag: 'Var'
    name: string
} | {
    tag: 'Func'
    param: Ty
    ret: Ty
} | {
    tag: 'Forall',
    alpha: string,
    ty: Ty
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
    body: Block
} | {
    tag: 'Anno'
    expr: Expr
    ty: Ty
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

// export type AST = {
//     stmts: Stmt
// }

export type AST = {
    stmt: Stmt
}

export function ppExpr(expr: Expr): string {
    return (new PP()).ppExpr(expr)
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
        // return ast.stmts.map(stmt => this.ppStmt(stmt)).join('\n')
        return this.ppStmt(ast.stmt)
    }

    public ppExpr(expr: Expr): string {
        switch (expr.tag) {
        case 'Abs': return `(${expr.param} -> ${this.ppExpr(expr.body)})`
        case 'Var': return expr.name
        case 'Lit': {
            switch (expr.kind.tag) {
            case 'Unit': return '()'
            case 'Bool': return expr.kind.val.toString()
            case 'Int': return expr.kind.val.toString()
            case 'String': return expr.kind.val
            }
            break
        }
        case 'Anno': return `${this.ppExpr(expr.expr)}: ${this.ppTy(expr.ty)}`
        case 'App': return `${this.ppExpr(expr.lhs)} ${this.ppExpr(expr.arg)}`
        case 'Block': return this.ppBlock(expr.block)
        case 'Let': return `let ${this.ppBlock(expr.body)}`
        }
    }

    public ppBlock(block: Block): string {
        this.indent()

        const s = block
            .stmts
            .map(stmt => `${this.indent_str()}${this.ppStmt(stmt)}`)
            .join('\n')

        const e = `${this.indent_str()}${this.ppExpr(block.expr)}`

        this.dedent()

        return `${block.stmts.length ? '\n' : ''}${s}\n${e}`
    }

    public ppStmt(stmt: Stmt): string {
        switch (stmt.tag) {
        case 'Expr': return this.ppExpr(stmt.expr)
        case 'Item': return this.ppItem(stmt.item)
        }
    }

    public ppItem(item: Item): string {
        switch (item.tag) {
        case 'Term': {
            return `${item.name} ${item.params.length ? ' ' : ''}${item.params.join(' ')}= ${this.ppExpr(item.body)}`
        }
        case 'Ty': return `type ${item.name} = ${item.ty}`
        }
    }

    public ppTy(ty: Ty): string {
        switch (ty.tag) {
        case 'Lit': {
            switch (ty.kind.tag) {
            case 'Unit': return '()'
            case 'Bool': return 'Bool'
            case 'Int': return 'Int'
            case 'String': return 'String'
            }
            break
        }
        case 'Func': return `${ty.param} -> ${ty.ret}`
        case 'Var': return ty.name
        case 'Forall': return `forall ${ty.alpha}. ${this.ppTy(ty.ty)}`
        }
    }
}
