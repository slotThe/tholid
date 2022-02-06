(define fib (n)
  (cond ((= n 0) 0)
        ((< n 3) 1)
        (#t (+ (fib (- n 1)) (fib (- n 2))))))

(fib 10)
