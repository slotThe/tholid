;; Essential

(define id (x)
  x)

(define not (x)
  (if x nil #t))

(define-syntax when (this body)
  `(if ,this (progn ,@body)))

;; Arithmetic

(define inc (x)
  (+ x 1))

(define dec (x)
  (- x 1))

;; List utilities

(define empty? (xs)
  (= nil (cdr xs)))

(define foldr (f def xs)
  (if (empty? xs)
      def
      (f (car xs)
         (foldr f def (cdr xs)))))

(define map (f xs)
  (foldr (lambda (a b) (cons (f a) b))
         '() xs))

(define filter (f xs)
  (foldr (lambda (a b) (if (f a) (cons a b) b))
         '() xs))
