# eas-geo-module: LLVM fuses sin and cos on Apple targets (not a neomacs bug)

`eas-geo-native-projections-match-lisp` failed on GNU Emacs 30.2 and neomacs
alike once `lib/eas-geo-module.dylib` was built on arm64 macOS (622 problems,
first at albers, Sphere, path index 58: Elisp `177.57492869074176`, module
`177.5749286907418`). Both Emacsen agree with each other: the module was off.

Cause: `x.sin()` and `x.cos()` of one `x` compile to one `__sincos_stret` call
on aarch64-apple-darwin, and its sin differs by 1 ulp from libm `sin` (what
Emacs calls) for 79 of 20000 random x in (-3, 3). Hiding the operand with
`std::hint::black_box` (`math::Libm::lsin`/`lcos`) keeps two libm calls; 0 of
20000 differ then. The test skips when the module is absent, so Linux CI never
saw it.
