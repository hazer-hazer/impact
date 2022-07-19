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
}
