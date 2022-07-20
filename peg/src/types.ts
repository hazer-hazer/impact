export class Result<T, E = Error> {
    public readonly ok: boolean
    private val: T | E

    private constructor(ok: boolean, val: T | E) {
        this.ok = ok
        this.val = val
    }

    public static Ok<T, E>(val: T): Result<T, E> {
        return new Result<T, E>(true, val)
    }

    public static Err<T, E>(err: E): Result<T, E> {
        return new Result<T, E>(false, err)
    }

    public unwrap(msg = 'Unwrapped error result'): T {
        if (this.ok) {
            return this.val as T
        }
        throw new Error(msg)
    }

    public unwrapErr(msg = 'Unwrapped ok result'): E {
        if (!this.ok) {
            return this.val as E
        }
        throw new Error(msg)
    }

    public unwrapWith(th: any): T {
        if (this.ok) {
            return this.val as T
        }
        throw th
    }

    public map<U>(mapOk: (val: T) => U): Result<U, E> {
        if (this.ok) {
            return Result.Ok(mapOk(this.val as T))
        }
        return Result.Err(this.val as E)
    }

    public mapErr<G>(map: (err: E) => G): Result<T, G> {
        if (!this.ok) {
            return Result.Err(map(this.val as E))
        }
        return Result.Ok(this.val as T)
    }
}
