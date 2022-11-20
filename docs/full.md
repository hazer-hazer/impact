# Impact

## Primitive types

> All types in **Impact** are written in PascalCase.

### Integer literals

- `I8`
- `I16`
- `I32`
- `I64`
- `I128` (?)

- `U8`
- `U6`
- `U32`
- `U64`
- `U128` (?)


### Float literals

- `F32`
- `F64`

### Character type

`Char`

### String type

`String`

### Unit type

`()`

### Bottom (never) type

`Never`

### Boolean type

For now, boolean is planned to be just an ADT.

```rust
enum Bool {
    True,
    False,
}
```

### Tuple type

`(T1, T2, T3, ..., TN)`

### List type

`[T]`

### Array type

Array type will be added as soon as const parameters are implemented, and it'll be a std type, not built-in.
We need const parameters for structure as follows: `Array<T, N>` where `T` is a type and `N` is a const (compile-time known) integer -- array size.

### View (slice) types

As for array, view types are std types, not built-in.
We need two view types: mutable one and immutable one.

I peeked name `View` for ease of distinction between of actual slice (when we slice an array for example) and data structure with pointer and size (view).

## Functions

```

```
