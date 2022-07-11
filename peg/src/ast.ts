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
    val: Expr
} | {
    tag: 'App'
    lhs: Expr
    rhs: Expr
} | {
    tag: 'Let'
    name: string
    val: Expr
    body: Expr
} | {
    tag: 'Anno'
    expr: Expr
    ty: Ty
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

export type Stmt = Expr | Item
