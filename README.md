# tholid

The obligatory lisp interpreter that everyone has to have done at least
once in their life.

## Examples

### Folds

``` scheme
  (define foldr (f def xs)
    (if (empty? xs)
        def
        (f (car xs)
           (foldr f def (cdr xs)))))

  (define sum (xs)
    (foldr + 0 xs)

  (sum '(1 2 3 4 5))
```

### Macros

``` scheme
  (define-syntax when (this body)
    `(if ,this (progn ,@body)))

  (define-syntax unless (this body)
    `(if ,this nil ,@body))

  ;; Proper &rest style functions are TODO.
  (when #t
    '((+ 1 2)
      (+ 3 5)))
```

# Installation and Usage

Build with `stack build` (or install via `stack install`).  By default,
the executable contains a simple repl.  For more ergonomic usage, you
can use it with [rlwrap]:

``` console
  $ stack build
  $ rlwrap stack exec -- tholid
```

[rlwrap]: https://github.com/hanslub42/rlwrap
