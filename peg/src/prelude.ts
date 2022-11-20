export function curry<T extends AnyFunc>(fn: T): AnyFunc {
    return function curried(this: any, ...args: any[]): any {
        if (args.length >= fn.length) {
            return fn.apply(this, args)
        }
        return function (this: any, ...suffix: any[]) {
            return curried.apply(this, [...args, ...suffix])
        }
    }
}

// Specifier for built-in in prelude, either a function func: (auto-curried) value
//  or a first-class built-in, i.e. type is inferred
type Decl = {
    tag: 'func'
    ty: string
    func: AnyFunc
} | {
    tag: 'val'
    ty: string
    val: any
}
export type Prelude = Record<string, Decl>

type AnyFunc = (...args: any[]) => any

const infixOpTy = 'forall t. t -> t -> t'
const cmpInfixOpTy = 'forall t. t -> t -> Bool'

export const firstClassPrelude = ''

const _prelude: Prelude = {
    print: {
        tag: 'func',
        ty: 'forall t. t -> ()',
        func: (a: any) => console.log(a),
    },

    // Basic math operators //
    '+': {
        tag: 'func',
        ty: infixOpTy,
        func: (lhs: any, rhs: any) => lhs + rhs,
    },
    '-': {
        tag: 'func',
        ty: infixOpTy,
        func: (lhs: any, rhs: any) => lhs - rhs,
    },
    '*': {
        tag: 'func',
        ty: infixOpTy,
        func: (lhs: any, rhs: any) => lhs * rhs,
    },
    '/': {
        tag: 'func',
        ty: infixOpTy,
        func: (lhs: any, rhs: any) => lhs / rhs,
    },

    // Comparison operators //
    '>': {
        tag: 'func',
        ty: cmpInfixOpTy,
        func: (lhs: any, rhs: any) => lhs > rhs,
    },
    '<': {
        tag: 'func',
        ty: cmpInfixOpTy,
        func: (lhs: any, rhs: any) => lhs < rhs,
    },
    '>=': {
        tag: 'func',
        ty: cmpInfixOpTy,
        func: (lhs: any, rhs: any) => lhs >= rhs,
    },
    '<=': {
        tag: 'func',
        ty: cmpInfixOpTy,
        func: (lhs: any, rhs: any) => lhs <= rhs,
    },
    '==': {
        tag: 'func',
        ty: cmpInfixOpTy,
        func: (lhs: any, rhs: any) => lhs == rhs,
    },
    '!=': {
        tag: 'func',
        ty: cmpInfixOpTy,
        func: (lhs: any, rhs: any) => lhs != rhs,
    },
}

export const prelude = Object.fromEntries(Object.entries(_prelude).map(([name, decl]): [string, Decl] => {
    switch (decl.tag) {
    case 'func': {
        return [name, {
            ...decl,
            func: curry(decl.func),
        }]
    }
    default: return [name, decl]
    }
}))
