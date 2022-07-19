# PEG syntax

This is a simple transpiler from Impact to JavaScript written with PeggyJS.
Now with type check.

It seems to run simple programs such as:
```haskell
fact n = if n <= 1 then 1 else n * fact (n - 1)
fib n = if n < 3 then 1 else fib (n - 1) + fib (n - 2)
```
