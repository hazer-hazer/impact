import { Constructor, Expr, Item, ppExpr, Stmt, Ty as AstTy } from './ast'
import { zip } from './utils'

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
} | {
    tag: 'Data'
    name: string
    types: Ty[]
} | {
    tag: 'Error'
}

export function ppTy(ty: Ty): string {
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
    case 'Existential': return `^${ty.name}`
    case 'Forall': return `forall ${ty.alpha}. ${ppTy(ty.ty)}`
    case 'Func': return `${ppTy(ty.param)} -> ${ppTy(ty.ret)}`
    case 'Data': return `${ty.name}${ty.types.length ? ' ' : ''}${ty.types.map(ppTy).join(' | ')}`
    case 'Error': return '[ERROR]'
    }
}

export function conv(astTy: AstTy, ctx: Ctx | null = null): Ty {
    switch (astTy.tag) {
    case 'Lit': {
        return {
            tag: 'Lit',
            kind: astTy.kind,
        }
    }
    case 'Func': {
        return {
            tag: 'Func',
            param: conv(astTy.param, ctx),
            ret: conv(astTy.ret, ctx),
        }
    }
    case 'Var': {
        return {
            tag: 'Var',
            name: astTy.name,
        }
    }
    case 'ConId': {
        if (!ctx) {
            throw new Error('Cannot convert AST type without context specified')
        }
        return ctx.resolveAlias(astTy.name)
    }
    case 'Forall': {
        return {
            tag: 'Forall',
            alpha: astTy.alpha,
            ty: conv(astTy.ty, ctx),
        }
    }
    }
}

function isMonotype(a: Ty): boolean {
    switch (a.tag) {
    case 'Func': return isMonotype(a.param) && isMonotype(a.ret)
    case 'Forall': return false
    default: return true
    }
}

function substitute(inTy: Ty, name: string, withTy: Ty): Ty {
    switch (inTy.tag) {
    case 'Lit': return inTy
    case 'Var': {
        if (inTy.name === name) {
            return withTy
        }
        return inTy
    }
    case 'Forall': {
        if (inTy.alpha === name) {
            return {
                tag: 'Forall',
                alpha: inTy.alpha,
                ty: withTy,
            }
        }
        return {
            tag: 'Forall',
            alpha: inTy.alpha,
            ty: substitute(inTy.ty, name, withTy),
        }
    }
    case 'Existential': {
        if (inTy.name === name) {
            return withTy
        }
        return inTy
    }
    case 'Func': return {
        tag: 'Func',
        param: substitute(inTy.param, name, withTy),
        ret: substitute(inTy.ret, name, withTy),
    }
    case 'Data': return {
        tag: 'Data',
        name: inTy.name,
        types: inTy.types.map(ty => substitute(ty, name, withTy)),
    }
    case 'Error': throw new InferErr('Cannot substitute error type')
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
    case 'Data': return a.types.some(ty => occursIn(name, ty))
    case 'Error': return false
    }
}

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
} | {
    tag: 'Alias'
    name: string
    ty: Ty
} | {
    tag: 'Cons'
    name: string
    ty: Ty
}

function ppCtxEl(el: CtxEl): string {
    switch (el.tag) {
    case 'Var': return el.name
    case 'Existential': return `^${el.name}${el.solution ? ` = ${ppTy(el.solution)}` : ''}`
    case 'Marker': return `>${el.name}`
    case 'TypedTerm': return `${el.name}: ${ppTy(el.ty)}`
    case 'Alias': return `type ${el.name} = ${ppTy(el.ty)}`
    case 'Cons': return `${el.name} ${ppTy(el.ty)}`
    }
}

export class InferErr extends Error {}

export class Ctx {
    constructor(private elements: CtxEl[] = []) { }

    private static lastExId = 0

    private static freshEx(): string {
        return (this.lastExId++).toString()
    }

    public pp(): string {
        return `Type context:\n${this.elements.map(el => `  ${ppCtxEl(el)}`).join('\n')}`
    }

    private clone(): Ctx {
        return Object.assign(Object.create(Object.getPrototypeOf(this)), this)
    }

    public add(el: CtxEl): Ctx {
        return new Ctx([...this.elements, el])
    }

    public addInPlace(el: CtxEl): this {
        this.elements.push(el)
        return this
    }

    public findTypedVar(name: string): Ty {
        const ty = this.elements.find(el => el.tag === 'TypedTerm' && el.name === name)
        if (ty?.tag === 'TypedTerm') {
            return ty.ty
        }
        throw new InferErr(`Type for ${name} not found`)
    }

    private findIndex(tag: CtxEl['tag'], name: string): number {
        return this.elements.findIndex(el => el.tag === tag && el.name === name)
    }

    private drop(el: CtxEl): Ctx {
        const index = this.findIndex(el.tag, el.name)
        if (index < 0) {
            throw new Error(`Failed to find context element ${ppCtxEl(el)} to drop`)
        }
        return new Ctx(this.elements.slice(0, index))
    }

    private split(el: CtxEl): CtxEl[] {
        const index = this.findIndex(el.tag, el.name)
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

    public resolveAlias(name: string): Ty {
        const index = this.findIndex('Alias', name)
        if (index < 0) {
            throw new InferErr(`Type ${name} not found`)
        }
        const el = this.elements[index]
        if (el.tag === 'Alias') {
            return el.ty
        }
        throw new Error()
    }

    private resolveCons(name: string): Ty {
        const index = this.findIndex('Cons', name)
        if (index < 0) {
            throw new InferErr(`Constructor ${name} not found`)
        }
        const el = this.elements[index]
        if (el.tag === 'Cons') {
            return el.ty
        }
        throw new Error()
    }

    // Figure 7.
    private isWf(ty: Ty): boolean {
        switch (ty.tag) {
        case 'Lit': return true
        case 'Var': return this.elements.some(el => el.tag === 'Var' && el.name === ty.name)
        case 'Func': return this.isWf(ty.param) && this.isWf(ty.ret)
        case 'Forall': return this.add({
            tag: 'Var',
            name: ty.alpha,
        }).isWf(ty.ty)
        case 'Existential': return this.elements.some(el => el.tag === 'Existential' && el.name === ty.name)
        case 'Data': return ty.types.every(ty => this.isWf(ty))
        case 'Error': return false
        }
    }

    // Figure 8.
    public apply(ty: Ty): Ty {
        switch (ty.tag) {
        case 'Var':
        case 'Lit': return ty
        case 'Existential': {
            for (const el of this.elements) {
                if (el.tag === 'Existential' && el.name === ty.name && el.solution !== null) {
                    return this.apply(el.solution)
                }
            }
            return ty
        }
        case 'Forall': return {
            tag: 'Forall',
            alpha: ty.alpha,
            ty: this.apply(ty.ty),
        }
        case 'Func': return {
            tag: 'Func',
            param: this.apply(ty.param),
            ret: this.apply(ty.ret),
        }
        case 'Data': return {
            tag: 'Data',
            name: ty.name,
            types: ty.types.map(this.apply.bind(this)),
        }
        case 'Error': throw new InferErr('Cannot apply context to error type')
        }
    }

    // Figure 9.
    private subtype(a: Ty, b: Ty): Ctx {
        if (!this.isWf(a) || !this.isWf(b)) {
            throw new InferErr(`Types \`${ppTy(a)}\` and \`${ppTy(b)}\` are ill-formed`)
        }

        // Idk what's wrong with typescript inference on switch-case on tuples,
        //  so I have to use if-else
        if (a.tag === 'Lit' && b.tag === 'Lit' && a.kind.tag === b.kind.tag) {
            return this.clone()
        }

        if (a.tag === 'Var' && b.tag === 'Var') {
            if (a.name === b.name) {
                return this.clone()
            }
        }

        if (a.tag === 'Existential' && b.tag === 'Existential') {
            if (a.name === b.name) {
                return this.clone()
            }
        }

        if (a.tag === 'Func' && b.tag === 'Func') {
            const theta = this.subtype(a.param, b.param)
            return theta.subtype(theta.apply(a.ret), theta.apply(b.ret))
        }

        if (a.tag === 'Forall') {
            const name = Ctx.freshEx()
            const gamma = this
                .add({tag: 'Marker', name})
                .add({tag: 'Existential', name, solution: null})

            return gamma.subtype(substitute(a.ty, a.alpha, {
                tag: 'Existential',
                name,
            }), b).drop({
                tag:'Marker',
                name,
            })
        }

        if (b.tag === 'Forall') {
            return this.add({
                tag: 'Var',
                name: b.alpha,
            }).subtype(a, b.ty)
                .drop({
                    tag: 'Var',
                    name: b.alpha,
                })
        }

        if (a.tag === 'Existential') {
            if (!occursIn(a.name, b)) {
                return this.instantiateL(a.name, b)
            }
            throw new InferErr('Circular type')
        }

        if (b.tag === 'Existential') {
            if (!occursIn(b.name, a)) {
                return this.instantiateR(a, b.name)
            }
            throw new InferErr('Circular type')
        }

        if (a.tag === 'Data' && b.tag === 'Data') {
            const tyPairs = zip(a.types, b.types)
            return tyPairs.reduce((ctx, [a, b]) => {
                const c = ctx.subtype(a, b)
                return c.subtype(b, a)
            }, this.clone())
        }

        throw new InferErr(`${ppTy(a)} is not a subtype of ${ppTy(b)}`)
    }

    subtypePair(lty: Ty, rty: Ty): Ctx {
        return this.subtype(this.apply(lty), this.apply(rty))
    }

    // Figure 10.
    private instantiateL(name: string, b: Ty): Ctx {
        if (isMonotype(b)) {
            return this.replace({
                tag: 'Existential',
                name,
                solution: null,
            }, [{
                tag: 'Existential',
                name,
                solution: b,
            }])
        }

        switch (b.tag) {
        case 'Func': {
            const alpha1 = Ctx.freshEx()
            const alpha2 = Ctx.freshEx()
            const gamma = this.replace({
                tag: 'Existential',
                name,
                solution: null,
            }, [{
                tag: 'Existential',
                name: alpha2,
                solution: null,
            }, {
                tag: 'Existential',
                name: alpha1,
                solution: null,
            }, {
                tag: 'Existential',
                name,
                solution: {
                    tag: 'Func',
                    param: {
                        tag: 'Existential',
                        name: alpha1,
                    },
                    ret: {
                        tag: 'Existential',
                        name: alpha2,
                    },
                },
            }])

            const theta = gamma.instantiateR(b.param, alpha1)
            const delta = theta.instantiateL(alpha2, theta.apply(b.ret))
            return delta
        }
        case 'Forall': {
            return this
                .add({
                    tag: 'Var',
                    name: b.alpha,
                })
                .instantiateL(name, b.ty)
                .drop({
                    tag: 'Var',
                    name: b.alpha,
                })
        }
        case 'Existential': {
            return this.replace({
                tag: 'Existential',
                name: b.name,
                solution: null,
            }, [{
                tag: 'Existential',
                name: b.name,
                solution: {
                    tag: 'Existential',
                    name,
                },
            }])
        }
        }

        throw new InferErr(`Cannot instantiateL ${name} as ${ppTy(b)}`)
    }

    // Figure 10.
    private instantiateR(a: Ty, name: string): Ctx {
        if (isMonotype(a)) {
            return this.replace({
                tag: 'Existential',
                name,
                solution: null,
            }, [{
                tag: 'Existential',
                name,
                solution: a,
            }])
        }

        switch (a.tag) {
        case 'Func': {
            const alpha1 = Ctx.freshEx()
            const alpha2 = Ctx.freshEx()

            const gamma = this.replace({
                tag: 'Existential',
                name,
                solution: null,
            }, [{
                tag: 'Existential',
                name: alpha2,
                solution: null,
            }, {
                tag: 'Existential',
                name: alpha1,
                solution: null,
            }, {
                tag: 'Existential',
                name,
                solution: {
                    tag: 'Func',
                    param: {
                        tag: 'Existential',
                        name: alpha1,
                    },
                    ret: {
                        tag: 'Existential',
                        name: alpha2,
                    },
                },
            }])

            const theta = gamma.instantiateL(alpha1, a.param)
            const delta = theta.instantiateR(theta.apply(a.ret), alpha2)
            return delta
        }
        case 'Forall': {
            const beta1 = Ctx.freshEx()
            const gamma = this.add({
                tag: 'Marker',
                name: beta1,
            }).add({
                tag: 'Existential',
                name: beta1,
                solution: null,
            })
            return gamma.instantiateR(substitute(a.ty, a.alpha, {
                tag: 'Existential',
                name: beta1,
            }), name).drop({
                tag: 'Marker',
                name: beta1,
            })
        }
        case 'Existential': {
            return this.replace({
                tag: 'Existential',
                name: a.name,
                solution: null,
            }, [{
                tag: 'Existential',
                name: a.name,
                solution: {
                    tag: 'Existential',
                    name,
                },
            }])
        }
        }

        throw new InferErr(`Cannot instantiateR ${name} as ${ppTy(a)}`)
    }

    private check(expr: Expr, ty: Ty): Ctx {
        if (expr.tag === 'Lit' && ty.tag === 'Lit') {
            if (expr.kind.tag !== ty.kind.tag) {
                throw new InferErr(`${ppExpr(expr)} is not of literal type ${ppTy(ty)}`)
            }
            return this
        }

        if (expr.tag === 'Abs' && ty.tag === 'Func') {
            const typedParam: CtxEl = {
                tag: 'TypedTerm',
                name: expr.param,
                ty: ty.param,
            }
            return this
                .add(typedParam)
                .check(expr.body, ty.ret)
                .drop(typedParam)
        }

        if (ty.tag === 'Forall') {
            const alpha: CtxEl = {
                tag: 'Var',
                name: ty.alpha,
            }
            return this
                .add(alpha)
                .check(expr, ty.ty)
                .drop(alpha)
        }

        const [synthTy, theta] = this.synthExpr(expr)
        return theta.subtype(theta.apply(synthTy), theta.apply(ty))
    }

    public synthStmt(stmt: Stmt): [Ty, Ctx] {
        switch (stmt.tag) {
        case 'Expr': return this.synthExpr(stmt.expr)
        case 'Item': return this.synthItem(stmt.item)
        }
    }

    private synthExpr(expr: Expr): [Ty, Ctx] {
        switch (expr.tag) {
        case 'Lit': {
            return [{
                tag: 'Lit',
                kind: expr.kind,
            }, this.clone()]
        }
        case 'Var': {
            for (const el of this.elements) {
                if (el.tag === 'TypedTerm' && el.name === expr.name) {
                    return [el.ty, this.clone()]
                }
            }
            throw new InferErr(`${ppExpr(expr)} is not defined`)
        }
        case 'Anno': {
            const ty = conv(expr.ty, this)
            const delta = this.check(expr.expr, ty)
            return [
                ty,
                delta,
            ]
        }
        case 'Abs': {
            const alpha = Ctx.freshEx()
            const beta = Ctx.freshEx()

            const typedParam: CtxEl = {
                tag: 'TypedTerm',
                name: expr.param,
                ty: {
                    tag: 'Existential',
                    name: alpha,
                },
            }

            const delta = this
                .add({
                    tag: 'Existential',
                    name: alpha,
                    solution: null,
                })
                .add({
                    tag: 'Existential',
                    name: beta,
                    solution: null,
                })
                .add(typedParam)
                .check(expr.body, {
                    tag: 'Existential',
                    name: beta,
                })
                .drop(typedParam)

            return [{
                tag: 'Func',
                param: {
                    tag: 'Existential',
                    name: alpha,
                },
                ret: {
                    tag: 'Existential',
                    name: beta,
                },
            }, delta]
        }
        case 'Let': {
            const letCtx = expr.body.stmts.reduce((ctx, stmt): Ctx => {
                if (stmt.tag === 'Expr') {
                    // Not last expression statement type is discarded
                    return ctx.synthExpr(stmt.expr)[1]
                }

                return ctx.synthItem(stmt.item)[1]
            }, this.clone())

            const [ty] = letCtx.synthExpr(expr.body.expr)
            return [
                ty,
                this.clone(),
            ]
        }
        case 'App': {
            const [ty, theta] = this.synthExpr(expr.lhs)
            return theta.appSynth(theta.apply(ty), expr.arg)
        }
        case 'If': {
            const branchesEx = Ctx.freshEx()
            const condCtx = this.check(expr.cond, {
                tag: 'Lit',
                kind: {tag: 'Bool'},
            }).addMany([{
                tag: 'Marker',
                name: branchesEx,
            }, {
                tag: 'Existential',
                name: branchesEx,
                solution: null,
            }])

            const thenCtx = condCtx.check(expr.then, {
                tag: 'Existential',
                name: branchesEx,
            })

            const elseCtx = thenCtx.check(expr.else, thenCtx.apply({
                tag: 'Existential',
                name: branchesEx,
            }))

            return [elseCtx.apply({
                tag: 'Existential',
                name: branchesEx,
            }), elseCtx.drop({
                tag: 'Marker',
                name: branchesEx,
            })]
        }
        case 'Cons': {
            const ty = this.resolveCons(expr.cons)
            return [ty, this]
        }
        }

        throw new InferErr(`Cannot synthesize type for expression ${ppExpr(expr)}`)
    }

    private appSynth(calleeTy: Ty, arg: Expr): [Ty, Ctx] {
        switch (calleeTy.tag) {
        case 'Existential': {
            const alpha1 = Ctx.freshEx()
            const alpha2 = Ctx.freshEx()

            const gamma = this.replace({
                tag: 'Existential',
                name: calleeTy.name,
                solution: null,
            }, [{
                tag: 'Existential',
                name: alpha2,
                solution: null,
            }, {
                tag: 'Existential',
                name: alpha1,
                solution: null,
            }, {
                tag: 'Existential',
                name: calleeTy.name,
                solution: {
                    tag: 'Func',
                    param: {
                        tag: 'Existential',
                        name: alpha1,
                    },
                    ret: {
                        tag: 'Existential',
                        name: alpha2,
                    },
                },
            }])

            const delta = gamma.check(arg, {
                tag: 'Existential',
                name: alpha1,
            })

            return [{
                tag: 'Existential',
                name: alpha2,
            }, delta]
        }
        case 'Forall': {
            const alpha1 = Ctx.freshEx()
            return this.add({
                tag: 'Existential',
                name: alpha1,
                solution: null,
            }).appSynth(substitute(calleeTy.ty, calleeTy.alpha, {
                tag: 'Existential',
                name: alpha1,
            }), arg)
        }
        case 'Func': {
            return [
                calleeTy.ret,
                this.check(arg, calleeTy.param),
            ]
        }
        }

        throw new InferErr(`Cannot synth application ty with callee ${ppTy(calleeTy)} and argument ${ppExpr(arg)}`)
    }

    private synthItem(item: Item): [Ty, Ctx] {
        switch (item.tag) {
        case 'Ty': {
            // TODO: Maybe return Unit type?
            const ty = conv(item.ty, this)
            return [
                ty,
                this.add({
                    tag: 'Alias',
                    name: item.name,
                    ty,
                }),
            ]
        }
        case 'Term': {
            let body: Expr = item.body
            if (item.params.length) {
                body = item.params.reduce((body, param) => ({
                    tag: 'Abs',
                    param,
                    body,
                }), item.body)
            }

            // This is what I came with, I'm not sure that this is valid for recursion
            // but it seems to work
            const ctx = this.add({
                tag: 'TypedTerm',
                name: item.name,
                ty: {
                    tag: 'Existential',
                    name: item.name,
                },
            })

            const [termTy, termCtx] = ctx.synthExpr(body)

            return [termTy, termCtx]
        }
        case 'Data': {
            // TODO: Add message with duplicate constructors names
            if (new Set(item.cons.map(c => c.name)).size !== item.cons.length) {
                throw new InferErr('Duplicate constructors found')
            }

            const consTypes = item.cons.map(con => this.conType(item.name, item.tyParams, con))

            const ctx = consTypes.reduce((ctx, ty, i) => ctx.add({
                tag: 'Cons',
                name: item.cons[i].name,
                ty,
            }), this.clone())

            return [{
                tag: 'Data',
                name: item.name,
                types: consTypes,
            }, ctx]
        }
        }
    }

    private conType(declName: string, tyParams: string[], con: Constructor): Ty {
        return tyParams.reduceRight((forall, param): Ty => ({
            tag: 'Forall',
            alpha: param,
            ty: forall,
        }), con.types.reduceRight((arr, ty): Ty => ({
            tag: 'Func',
            param: arr,
            ret: conv(ty),
        }), <Ty>{
            tag: 'Data',
            name: declName,
            types: tyParams.map(name => ({
                tag: 'Var',
                name,
            })),
        }))
    }
}
