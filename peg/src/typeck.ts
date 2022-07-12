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
    solution: Ty | null
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

    // Figure 7.
    public is_wf(ty: Ty): boolean {
        switch (ty.tag) {
            case 'Lit': return true
            case 'Var': return this.elements.some(el => el.tag === 'Var' && el.name === ty.name)
            case 'Func': return this.is_wf(ty.param) && this.is_wf(ty.ret)
            case 'Forall': return this.add({
                tag: 'Var',
                name: ty.alpha,
            }).is_wf(ty.ty)
            case 'Existential': return this.elements.some(el => el.tag === 'Existential' && el.name === ty.name)
        }
    }

    // Figure 8.
    public apply(ty: Ty): Ty {
        switch (ty.tag) {
            case 'Var':
            case 'Lit': return ty
            case 'Existential': return ty.solution || ty
            case 'Forall': return {
                tag: 'Forall',
                alpha: ty.alpha,
                ty: this.apply(ty.ty)
            }
            case 'Func': return {
                tag: 'Func',
                param: this.apply(ty.param),
                ret: this.apply(ty.ret)
            }
        }
    }

    public subtype(a: Ty, b: Ty): Ctx {
        switch ([a.tag, b.tag]) {
            case ['Lit', 'Lit']: return {...this}
            case ['Var', 'Var']: {
                if (this.is_wf(a))
            }
        }
    }
}
