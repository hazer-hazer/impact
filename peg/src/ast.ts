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
    tag: 'ConId',
    name: string,
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
    tag: 'Lit'
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
    tag: 'Block'
    block: Block
} | {
    tag: 'If'
    cond: Expr
    then: Expr
    else: Expr
} | {
    tag: 'Cons'
    cons: string
    args: Expr[]
}

export type Constructor = {
    name: string
    types: Ty[]
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
} | {
    tag: 'Data'
    name: string
    tyParams: string[]
    cons: Constructor[]
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

export function ppTy(ty: Ty): string {
    return (new PP()).ppTy(ty)
}

// PP //
export class PP {
    constructor(private indentLevel: number = 0) { }

    private indent() {
        this.indentLevel++
    }

    private dedent() {
        this.indentLevel--
    }

    private indentStr() {
        return '  '.repeat(this.indentLevel)
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
        case 'App': return `(${this.ppExpr(expr.lhs)} ${this.ppExpr(expr.arg)})`
        case 'Block': return this.ppBlock(expr.block)
        case 'Let': return `let ${this.ppBlock(expr.body)}`
        case 'If': return `if ${this.ppExpr(expr.cond)} then ${this.ppExpr(expr.then)} else ${this.ppExpr(expr.else)}`
        case 'Cons': return `${expr.cons} ${expr.args.map(ppExpr).join(' ')}`
        }
    }

    public ppBlock(block: Block): string {
        this.indent()

        const s = block
            .stmts
            .map(stmt => `${this.indentStr()}${this.ppStmt(stmt)}`)
            .join('\n')

        const e = `${this.indentStr()}${this.ppExpr(block.expr)}`

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
            return `${item.name} ${item.params.join(' ')}${item.params.length ? ' ' : ''}= ${this.ppExpr(item.body)}`
        }
        case 'Ty': return `type ${item.name} = ${ppTy(item.ty)}`
        case 'Data': {
            this.indent()

            const cons = item.cons.map(c => `${this.indentStr()}| ${c.name} ${c.types.map(ppTy)}`).join('\n')

            this.dedent()

            return `data ${item.name} =\n${cons}`
        }
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
        case 'Func': return `${ppTy(ty.param)} -> ${ppTy(ty.ret)}`
        case 'Var': return ty.name
        case 'ConId': return ty.name
        case 'Forall': return `forall ${ty.alpha}. ${this.ppTy(ty.ty)}`
        }
    }
}
