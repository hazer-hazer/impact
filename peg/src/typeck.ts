type Ty = {
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
    tag: 'Existential'
    name: string
} | {
    tag: 'Forall'
    alpha: string
    ty: Ty
} | {
    tag: 'Func'
    param: Ty
    ret: Ty
}

type CtxEl = {
    tag: 'Var'
    name: string
} | {
    tag: 'Existential'
    name: string
} | {
    tag: 'Solved'
    name: string
    ty: Ty
} | {
    tag: 'Marker'
    name: string
} | {
    tag: 'TypedTerm'
    name: string
    ty: Ty
}

class Ctx {
    constructor(private elements: CtxEl[] = []) { }

    public add(el: CtxEl): Ctx {
        return new Ctx([...this.elements, el])
    }
}


