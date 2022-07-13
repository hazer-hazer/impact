abstract class Type {
    abstract readonly tag: 'Lit' | 'Var' | 'Existential' | 'Forall' | 'Func'

    public abstract toString(): string;
}

class Lit extends Type {
    readonly tag = 'Lit'
    
    constructor(public readonly kind: 'Unit' | 'Bool' | 'Int' | 'String') {
        super()
    }

    public toString(): string {
        return this.kind
    }
}

class Var extends Type {
    readonly tag = 'Var'

    constructor(public readonly name: string) {
        super()
    }

    public toString(): string {
        return this.name
    }
}

class Existential extends Type {
    readonly tag = 'Existential'

    constructor(public readonly name: string, public readonly solution: Type | null) {
        super()
    }

    public toString(): string {
        return `^${this.name}${this.solution ? ` = ${this.solution.toString()}` : ''}`
    }
}

class Forall extends Type {
    readonly tag = 'Forall'

    constructor(public readonly alpha: string, public readonly ty: Type) {
        super()
    }

    public toString(): string {
        return `forall ${this.alpha}. ${this.ty.toString()}`
    }
}

class Func extends Type {
    readonly tag = 'Func'

    constructor(public readonly param: string, public readonly ret: Type) {
        super()
    }

    public toString(): string {
        return `${this.param.toString()} -> ${this.ret.toString()}`
    }
}
