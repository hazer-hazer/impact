# Impact programming language

This is a toy programming language meant to practice some new topics for me:
- Implementing programming language in Rust from scratch
- Bidirectional type checking
- Algebraic effects
- [*] Higher-kinded types
- [*] Linear types

[*] This topics might be excluded from the project if do not match with other features or making it too complex to KISS.

Comparing with my primary project -- [_Jacy_](https://github.com/jacylang/jacy) which I got stuck with because of initial complexity (it's about 20k sloc but even type check is not done 😪), in _Impact_ I'm trying to go with KISS from start to the end, even considering the fact that I'll try to cover so many topics.

### References

This list contains all reference I used to implement different stages and features.

Of course, I've needed Rust documentation along with `rustc` internals (rust is the best large programming language which source code I can read and even understand something)
- [Rust std documentation 😸](https://doc.rust-lang.org/std/)
- [`rustc` documentation and source](https://doc.rust-lang.org/stable/nightly-rustc/)
- [`rustc` book](https://doc.rust-lang.org/rustc/index.html)


#### Type checking

- ["Complete and Easy Bidirectional Typechecking
for Higher-Rank Polymorphism" (PDF)](https://arxiv.org/pdf/1306.6032.pdf) and its implementations
  - [**JS/TS version** multiple implementations](https://github.com/atennapel/bidirectional.js) by [Albert ten Napel / atennapel](https://github.com/atennapel)
  -  [**Rust version**](https://github.com/JDemler/BidirectionalTypechecking) by [Jakob Demler
 / JDemler](https://github.com/JDemler)
  - [**Rust version**](https://github.com/nikomatsakis/bidir-type-infer) by [Niko Matsakis
 / nikomatsakis](https://github.com/nikomatsakis) nico nico nii
  - [**Lean4 version**](https://github.com/gabriel-fallen/bidirectional-demo) by [Alexander Chichigin
 / gabriel-fallen](https://github.com/gabriel-fallen)


### Preliminary summary

#### Rust as a language for compiler dev

I began this project with one main point in development -- KISS, but no external tools were used which would help me with, for example, parsing.
The reason is that I wanted to try Rust as a replacement for C++ that I used in _Jacy_.
Right now, writing this, I'm implementing type check, parser stage is mostly done and the only thing I wanna say about using Rust as a programming language for compiler development is that it's freaking amazing. Really, I feel so good with it I would rewrite _Jacy_ in it.
