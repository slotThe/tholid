;; Arithmetic

(define inc (x)
  (+ x 1))

(define dec (x)
  (- x 1))

;; List utilities

(define empty? (xs)
  (= nil (car xs)))

(define map (f xs)
  (if (empty? xs)
      '()
      (cons (f (car xs))
            (map f (cdr xs)))))

(define foldr (f def xs)
  (if (empty? xs)
      def
      (f (car xs)
         (foldr f def (cdr xs)))))
