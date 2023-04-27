(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))


(define (list . objs) objs)


(define (id obj) obj)
(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g) (lambda (arg) (f (apply g args))))


(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))


(define (foldr func end ls)
  (if (null? ls)
      end
      (func (car ls) (foldr func end (cdr ls)))))

(define (foldl func accum ls)
  (if (null? ls)
      accum
      (foldl func (func accum (car ls)) (cdr ls))))

(define fold foldl)
(define reduce fold)

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))


(define (sum . ls) (fold + 0 ls))
(define (product . ls) (fold * 1 ls))
(define (and . ls) (fold && #t ls))
(define (or . ls) (fold || #f ls))


(define (max ls) (fold (lambda (old new) (if (> old new) old new)) (car ls) (cdr ls)))
(define (min ls) (fold (lambda (old new) (if (< old new) old new)) (car ls) (cdr ls)))


(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj ls) (fold (mem-helper (curry eq? obj) id) #f ls))
(define (memv obj ls) (fold (mem-helper (curry eqv? obj) id) #f ls))
(define (member obj ls) (fold (mem-helper (curry equal? obj) car) #f ls))
(define (assq obj alist) (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist) (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))


(define (length ls) (fold (lambda (x y) (+ x 1)) 0 ls))
(define (reverse ls) (fold (flip cons) '() ls))


(define (map func ls) (foldr (lambda (x y) (cons (func x) y)) '() ls))
(define (filter pred ls) (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() ls))


(define (list-tail ls k)
  (if (zero? k)
      ls
      (list-tail (cdr ls) (- k 1))))


(define (list-ref lst k) (car (list-tail lst k)))
(define (append . lists) (foldr (lambda (x y) (foldr cons y x)) '() lists))
