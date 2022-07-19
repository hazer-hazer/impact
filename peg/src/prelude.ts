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

export const prelude: Prelude = {
    print: [
        'forall t. t -> ()',
        curry(
            (a: any) => console.log(a),
        ),
    ],
    '+': [
        'forall t. t -> t -> t',
        curry(
            (lhs: any, rhs: any) => lhs + rhs,
        ),
    ],
}
