export type Prelude = Record<string, [string, any]>

export const prelude: Prelude = {
    print: [
        'forall t. t -> t',
        (a: any) => console.log(a),
    ],
    '+': [
        'forall t. t -> t -> t',
        (lhs: any, rhs: any) => lhs + rhs,
    ],
}
