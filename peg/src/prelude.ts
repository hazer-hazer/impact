export type Prelude = Record<string, [string, any]>

type AnyFunc = (...args: any[]) => any

export function curry<T extends AnyFunc>(fn: T): AnyFunc {
    return function curried(this: any, ...args: any[]) {
        if (args.length >= fn.length) {
            return fn.apply(this, args)
        }
        return function(this: any, ...suffix: any[]) {
            return curried.apply(this, [...args, ...suffix])
        }
    }
}

const infixOpTy = 'forall t. t -> t -> t'
const cmpInfixOpTy = 'forall t. t -> t -> Bool'

export const prelude: Prelude = {
    print: [
        'forall t. t -> ()',
        curry(
            (a: any) => console.log(a),
        ),
    ],

    // Basic math operators //
    '+': [
        infixOpTy,
        curry(
            (lhs: any, rhs: any) => lhs + rhs,
        ),
    ],
    '-': [
        infixOpTy,
        curry(
            (lhs: any, rhs: any) => lhs - rhs,
        ),
    ],
    '*': [
        infixOpTy,
        curry(
            (lhs: any, rhs: any) => lhs * rhs,
        ),
    ],
    '/': [
        infixOpTy,
        curry(
            (lhs: any, rhs: any) => lhs / rhs,
        ),
    ],

    // Comparison operators //
    '>': [
        cmpInfixOpTy,
        curry(
            (lhs: any, rhs: any) => lhs > rhs,
        ),
    ],
    '<': [
        cmpInfixOpTy,
        curry(
            (lhs: any, rhs: any) => lhs < rhs,
        ),
    ],
    '>=': [
        cmpInfixOpTy,
        curry(
            (lhs: any, rhs: any) => lhs >= rhs,
        ),
    ],
    '<=': [
        cmpInfixOpTy,
        curry(
            (lhs: any, rhs: any) => lhs <= rhs,
        ),
    ],
    '==': [
        cmpInfixOpTy,
        curry(
            (lhs: any, rhs: any) => lhs == rhs,
        ),
    ],
    '!=': [
        cmpInfixOpTy,
        curry(
            (lhs: any, rhs: any) => lhs != rhs,
        ),
    ],
}
