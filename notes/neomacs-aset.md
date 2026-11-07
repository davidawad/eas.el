# neomacs 0.0.20: `aset` cannot store a non-ASCII char into a string

Upstream repro (neomacs 0.0.20, commit 47fa15cc, aarch64-apple-darwin):

```sh
neomacs --batch -Q --eval '(let ((s (copy-sequence "abc"))) (aset s 1 #x2802) (princ s))'
emacs   --batch -Q --eval '(let ((s (copy-sequence "abc"))) (aset s 1 #x2802) (princ s))'
```

| call | GNU Emacs 30.2 | neomacs 0.0.20 |
|---|---|---|
| `(aset (copy-sequence "abc") 1 #x2802)` | `"a⠂c"` (string widened to multibyte in place) | `(error "Attempt to store non-byte value into unibyte string")` |
| `(aset (string-to-multibyte "abc") 1 #x2802)` | `"a⠂c"` | `(error "Attempt to store non-ASCII char into multibyte string")` |
| `(aset (copy-sequence "⠁bc") 1 #x2802)` | `"⠁⠂c"` | `(error "Attempt to store non-ASCII char into multibyte string")` |
| `(aset (copy-sequence "⠁bc") 0 ?a)` | `"abc"` | `(error "Attempt to replace non-ASCII char in multibyte string")` |
| `(aset (make-string 3 ?a) 1 #x2802)` | `"a⠂a"` | `(error "Attempt to store non-byte value into unibyte string")` |
| `(aset (copy-sequence "abc") 1 ?é)` | `"a\351c"` (unibyte) | same |

Expected: `aset` stores any character; storing a char of a different byte
width rebuilds the string's storage (GNU: `Faset` in data.c, string branch,
`STRING_SET_MULTIBYTE`/`string_set_multibyte` path). Only ASCII-for-ASCII
stores work in neomacs.

Workaround in eas: build the edited string with `concat`/`substring`
(see `eas-text-frames-test--aset`), carrying the char's text properties by hand.

Fails under neomacs because of this: `eas-text-frames-random-patches-equal-fresh-inserts`.
