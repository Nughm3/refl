# refl: reference language for compiler experiments

Compiler developers might sometimes want to experiment with new ideas, but implementing a new language from scratch each time is annoying, especially syntax analysis components, as they don't vary that much between language implementations.

`refl` is a simple toy "language" to experiment and run tests on. This repository contains a parser and AST API. ([ungrammar](https://rust-analyzer.github.io/blog/2020/10/24/introducing-ungrammar.html): [`refl.ungram`](/refl.ungram))

### Licensing

Licensed under the [MIT License](/LICENSE)
