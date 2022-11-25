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

## Definitions

### Variables

```hs
a = 1
b = "asd"
```

Name shadowing is allowed:
```hs
a = 1
a = a + 1
```

By the way, name shadowing is strictly warned if used not how I want :)
The only good way (of the ways I know about, of course) of using name shadowing in my opinion is to shadow variable using its value in new value, like in example above. By the way, it is okay for shadowed name to change type.

So these cases will produce warnings:
```hs
a = 1
a = 2

b = "a"
b = 2
```

### Functions

```hs
id x = x
```

Functions are transformed to variables with lambda values, but functions are resolved differently on name resolution stage. Functions are "declared" first and then "defined", hence it's possible to write mutually recursive functions or just declare function after its usage.

## Names

Before moving on to modules and other items, we need to talk about names.
Definitions (variables and functions) are always named in camelCase.
Names are stored in different namespaces: types namespace and value namespace, and, possibly, more specific namespaces will come in the future.
In most cases, it is clear which namespace use to find a specific name, e.g. in type annotation it is obvious that there's a type name, not a value.
In paths (`Something.Name`) the prefix, i.e. the part before the last element, always consist of names from type namespace, so in `Something.Name` we look for a `Something` name in type namespace, but to resolve `Name` we need to know where `Something.Name` appears.

### Naming convention

|            Name kind            |          Format          |
| :-----------------------------: | :----------------------: |
|     Type (except built-in)      |        PascalCase        |
|           ADT Variant           |        PascalCase        |
|              Trait              |        PascalCase        |
|         Type variables          |        camelCase         |
| Definitions (variable/function) |        camelCase         |
|           Module name           |        snake_case        |
|            File name            | snake_case or kebab-case |
|            Constants            |     UPPER_SNAKE_CASE     |

## Modules

Each file is a module, and should be named in snake_case (preferred) or in kebab-case.
Modules are also defined with `mod` keyword followed by name of the module also in snake_case.

Modules are containers for zero or more items: definitions, types, etc.

### Why snake_case?

At first, I don't like files named in PascalCase in not OOP languages. Of course, I accept file names like `AuthenticationController.ts` or something like that in OOP languages, but here there's no reason to do so.
So files are named in snake_case or kebab-case and modules do so, but we cannot use kebab-case because of problems with parsing (`-` operator), hence only snake_case is the case 😃.

But what about name resolution and paths? You might think "Oh, this would be disgusting to write paths like `Something.module_name.a`", but there are no paths like that. Nothing except modules can contain modules, hence only snake_case-named modules will appear in prefix of the path.

The second thing is visual recognition for a programmer.
Take a look at this path: `my_lib.object.Type.DEFAULT`.
Here, I'm sure that `my_lib` is a module, `object` is a module too, and `Type` is a type, `DEFAULT` is some constant.
I'm not still sure if associated types will come as a feature, but their paths would look like this `SomeType.AssocType.SuperAssocType`, where we know that these are all types, not modules.


