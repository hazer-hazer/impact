# Pretty-printing

> PP = Pretty-Printing

Pretty-printing needs to be strongly pre-specified, because it is used to check the stdout in source code tests.
I use `std::Display` for this, but maybe some custom trait like `PP` should be used in the future.

1. Each single structure that be printed in pretty-printers must always be printed the same way.
2. Printing of non-corelative lists of structures, e.g. list of tokens, need to be described.

## Atomic structures

Let's start with small structures going from first to last stages.

### Token

Token consists of two main parts: span and kind.
Whereas kind is a representation of source code token, span is meta information and never be printed in PP.
Briefly: Tokens are printed as they are in source code.

#### Error token

Error token is printed as `[ERROR]`.

#### List of tokens

Just a list of tokens (not as in AST as, for example, function parameters) is printed comma-delimited.

### AST

Briefly: All AST nodes are printed in format of source code.

- Indentation is 4-space.
- In white-space-delimited tokens single white-space is always used, e.g. function parameters are delimited with ` `.
- Infix and prefix operators are separated from LHS and RHS with single white-space.

#### Error Node

Error node is printed as `[ERROR]`.
