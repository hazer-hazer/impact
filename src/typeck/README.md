# Type checking



## Kinds

```rust
enum TyKind {
    // Simple types...

    Kind(Kind),
}

// Thinking of it like a type constructor
enum Kind {
    // Builtin type constructors
    RefCons,

    // User-defined type constructors
    Cons(NonZeroUsize),
    HigherOrder(Vec<Kind>),
}
```

`RefCons` is a built-in `Ref` type constructor accepting single argument.
`Cons` is a simple type constructor not only `NonZeroUsize` but actually non-one `usize`, because `Kind` without any parameter is actually just a `Type`. It has a form of `* -> ... -> *`
`HigherOrder` `Kind` is non-zero-length `Vec` of `Kind`s. It is considered a bug creating single-element `HigherOrder` `Kind` because for this case `Kind::Cons` must be used, has any form such as `* -> (* -> *) -> *`.

This structure only says about kind parameters count and not algorithmically usable for us.

### Evolving kinds structure

What do we need?
1. Kinds should be partially-inferrable, i.e. we infer one kind parameter then infer one more and so on.
2. Kinds parameters must be bound to a specific application, as type variables are bound to `ExprId` + `Type`.
3. Existential variables should be able to be solved to kind variables, but wait, by reading "Complete and easy bidirectional type checking" you can see that existentials are only solved to monotypes (kinds are not monotypes because they construct type which can be universally quantified). So we need separate type of existentials 😨 -- Kind existential.

Wow, this is a ton of work to do, let's start with a simplified structure.
```rust
// Abstracts not only around types but around kinds too.
enum TyKind {
    // Types...

    Kind(Kind),
}

enum KindKind {
    // The resulting type where all kinds are bound.
    // Cannot be `TyKind::Kind` (!)
    Ty(Ty),
    // Kind -> Kind, e.g. * -> * which is `KindKind::Ty -> KindKind::Ty`
    Abs(Kind, Kind),
    // Kind variable
    Var(KindVarId),
    // Kind existential
    Ex(KindEx),
    // Like `forall TyVar. Ty` but `forall KindVar. Kind`
    //  with which we can get for example `id_arr: forall 'k. ['k] -> ['k]`.
    //  Here, I used ' for kind annotation, but syntax is not established yet.
    Forall(KindVarId, Kind),
}
```

I want `Type` to be `Copy` type, thus we need `Kind::body` to be kind identifier. So `Kind` should be interned as `Type`.
