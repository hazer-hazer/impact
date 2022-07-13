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

function isMonotype(a: Ty): boolean {
    switch (a.tag) {
        case 'Func': return isMonotype(a.param) && isMonotype(a.ret)
        case 'Forall': return false
        default: return true
    }
}

function substitute(a: Ty, name: string, b: Ty): Ty {
    switch (a.tag) {
    case 'Lit': return a
    case 'Var': {
        if (a.name === name) {
            return b
        }
        return a
    }
    case 'Forall': {
        if (a.alpha === name) {
            return {
                tag: 'Forall',
                alpha: a.alpha,
                ty: b
            }
        }
        return {
            tag: 'Forall',
            alpha: a.alpha,
            ty: substitute(a.ty, name, b)
        }
    }
    case 'Existential': {
        if (a.name === name) {
            return b
        }
        return a
    }
    case 'Func': return {
        tag: 'Func',
        param: substitute(a.param, name, b),
        ret: substitute(a.ret, name, b)
    }
    }
}

function occursIn(name: string, a: Ty): boolean {
    switch (a.tag) {
    case 'Lit': return false
    case 'Var': return a.name === name
    case 'Func': return occursIn(name, a.param) || occursIn(name, a.ret)
    case 'Forall': {
        if (name === a.alpha) {
            return true
        }
        return occursIn(name, a.ty)
    }
    case 'Existential': return name === a.name
    }
}

function toString(ty: Ty): string {
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
    case 'Var': return ty.name
    case 'Existential': return `${ty.name}${ty.solution ? ` = ${toString(ty.solution)}` : ''}`
    case 'Forall': return `forall ${ty.alpha}. ${toString(ty.ty)}`
    case 'Func': return `${toString(ty.param)} -> ${toString(ty.ret)}`
    }
}

type TyTag = Ty['tag']

type CtxEl = {
    tag: 'Var'
    name: string
} | {
    tag: 'Existential'
    name: string
    solution: Ty | null
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

    private add(el: CtxEl): Ctx {
        return new Ctx([...this.elements, el])
    }

    private drop(el: CtxEl): Ctx {
        const index = this.elements.indexOf(el)
        if (index < 0) {
            throw new Error()
        }
        return new Ctx(this.elements.slice(0, index))
    }

    private split(el: CtxEl): CtxEl[] {
        const index = this.elements.indexOf(el)
        if (index < 0) {
            return []
        }
        const els = this.elements.splice(index)
        els.shift()
        return els
    }

    private addMany(els: CtxEl[]): this {
        this.elements.push(...els)
        return this
    }

    private replace(el: CtxEl, insert: CtxEl[]): this {
        const rightSide = this.split(el)
        this.addMany(insert)
        this.addMany(rightSide)
        return this
    }

    // Figure 7.
    private is_wf(ty: Ty): boolean {
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
    private apply(ty: Ty): Ty {
        switch (ty.tag) {
        case 'Var':
        case 'Lit': return ty
        case 'Existential': {
            if (this.elements.some(el => el.tag === 'Existential' && el.name === ty.name && el.solution))
        }
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

    // Figure 9.
    private subtype(a: Ty, b: Ty): Ctx {
        if (!this.is_wf(a) || !this.is_wf(b)) {
            throw new Error(`Types \`${toString(a)}\` and \`${toString(b)}\` are ill-formed`)
        }

        // Idk what's wrong with typescript inference on switch-case on tuples,
        //  so I have to use if-else
        if (a.tag === 'Lit' && b.tag === 'Lit') {
            return {...this}
        }

        if (a.tag === 'Var' && b.tag === 'Var') {
            if (a.name === b.name) {
                return {...this}
            }
        }

        if (a.tag === 'Existential' && b.tag === 'Existential') {
            if (a.name === b.name) {
                return {...this}
            }
        }

        if (a.tag === 'Func' && b.tag === 'Func') {
            const theta = this.subtype(a.param, b.param)
            return theta.subtype(theta.apply(a.ret), theta.apply(b.ret))
        }

        if (a.tag === 'Forall') {
            const name = '>a'
            const gamma = this
                .add({tag: 'Marker', name})
                .add({tag: 'Existential', name})
            
            return gamma.subtype(substitute(a.ty, a.alpha, {
                tag: 'Existential',
                name,
                solution: null,
            }), b).drop({
                tag:'Marker',
                name,
            })
        }

        if (b.tag === 'Forall') {
            return this.add({
                tag: 'Var',
                name: b.alpha
            }).subtype(a, b.ty).drop({
                tag: 'Var',
                name: b.alpha,
            })
        }

        if (a.tag === 'Existential') {
            if (!occursIn(a.name, b)) {
            }
        }

        if (b.tag === 'Existential') {

        }
    }

    private instantiateL(name: string, b: Ty): Ctx {
        if (isMonotype(b)) {
            return this.replace({
                tag: 'Existential',
                name,
            }, [{
                tag: 'Existential',
                name,
            }])
        }

        switch (b.tag) {
            case 'Func': {
                const alpha1 = '>a1'
                const alpha2 = '>a2'
                const gamma = 
            }
        }
    }
}
