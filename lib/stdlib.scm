(define (not x)           (if x #f #t))
(define (null? obj)       (if (eqv? obj '()) #t #f))

(define (id obj)          obj)
(define (list . objs)     objs)
(define (flip f)          (lambda (x y) (f y x)))
(define (curry f x)       (lambda (xs) (apply f (cons x (list xs)))))
(define (compose f g)     (lambda (xs) (f (apply g xs))))

(define (foldr f base xs)
    (if (null? xs)
        base
        (f (car xs) (foldr f base (cdr xs)))))

(define (foldl f acc xs)
    (if (null? xs)
        acc
        (foldl f (f acc (car xs)) (cdr xs))))

(define (reverse xs)      (foldl (flip cons) '() xs))
(define (length xs)       (foldl (lambda (len x) (+ len 1)) 0 xs))
(define (append xs ys)    (foldr (lambda (x zs) (cons x zs)) ys xs))
(define (concat xss)      (foldr (lambda (xs ys) (append xs ys)) '() xss))
(define (map f xs)        (foldr (lambda (x ys) (cons (f x) ys)) '() xs))
(define (filter p xs)     (foldr (lambda (x xs) (if (p x) (cons x xs) xs)) '() xs))
(define (flatmap f xs)    (foldl (lambda (ys x) (append ys (f x))) '() xs))

(define zero?             (curry = 0))
(define positive?         (curry < 0))
(define negative?         (curry > 0))
(define (odd? n)          (= (mod n 2) 1))
(define (even? n)         (= (mod n 2) 0))

(define (sum . xs)        (foldl + 0 xs))
(define (product . xs)    (foldl * 1 xs))
(define (and . xs)        (foldl && #t xs))
(define (or . xs)         (foldl || #f xs))
(define (max x . xs)      (foldl (lambda (a b) (if (> a b) a b)) x xs))
(define (min x . xs)      (foldl (lambda (a b) (if (< a b) a b)) x xs))

(define (mem-helper p op) (lambda (acc x) (if (and (not acc) (p (op x))) x acc)))
(define (memq obj xs)     (foldl (mem-helper (curry eq?    obj) id) #f xs))
(define (memv obj xs)     (foldl (mem-helper (curry eqv?   obj) id) #f xs))
(define (member obj xs)   (foldl (mem-helper (curry equal? obj) id) #f xs))
(define (assq obj xs)     (foldl (mem-helper (curry eq?    obj) car) #f xs))
(define (assv obj xs)     (foldl (mem-helper (curry eqv?   obj) car) #f xs))
(define (assoc obj xs)    (foldl (mem-helper (curry equal? obj) car) #f xs))