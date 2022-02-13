# tholid

The obligatory lisp interpreter that everyone has to have done at least
once in their life.

# Installation and Usage

Build with `stack build` (or install via `stack install`).  By default,
the executable contains a simple repl.  For more ergonomic usage, you
can use it with [rlwrap]:

``` console
  $ stack build
  $ rlwrap stack exec -- lisp
```

# TODO

- [ ] Actual error messages
- [ ] Actual type checking
- [ ] Macros
- [ ] A more robust parser


[rlwrap]: https://github.com/hanslub42/rlwrap
