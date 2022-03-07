;;;;;;;;;;;;; COMP 105 Typed uScheme: typed-funs.scm ;;;;;;;;;;;;;;;;;


;; Exercise TD: Polymorphic functions in Typed uScheme

(check-type drop 
    (forall ('a) (int [list 'a] -> [list 'a]))) 

(val drop
  (type-lambda ('a)
    (letrec
      [([drop-mono : (int (list 'a) -> (list 'a))]
           (lambda ([n  : int] [xs : (list 'a)])
             (if ([@ null? 'a] xs)
                 [@ '() 'a]
                 (if (> n 0)
                     (drop-mono (- n 1) ([@ cdr 'a] xs))
                     xs))))]
      drop-mono)))
             

        (check-expect ([@ drop int]  0 '(1 2 3 4))   '(1 2 3 4))
        (check-expect ([@ drop bool] 1 '(#f #t))     '(#t))
        (check-expect ([@ drop int]  3 [@ '() int])  [@ '() int])
        (check-expect ([@ drop sym]  2 '(Tufts Uni)) [@ '() sym])
        (check-expect ([@ drop int]  5 '(1 2 3 4))   [@ '() int])


(define bool even? ([x : int])
    ([@ = int] (mod x 2) 0))
                        

;; takewhile p? xs returns the longest prefix containing elements in list
;; `xs` that satisfies some predicate p?

(check-type takewhile
    (forall ('a) (('a -> bool) [list 'a] -> [list 'a])))

(val takewhile 
  (type-lambda ('a)
    (letrec
      [([takewhile-mono : (('a -> bool) (list 'a) -> (list 'a))]
           (lambda ([p? : ('a -> bool)] [xs : (list 'a)])
             (if ([@ null? 'a] xs)
                 [@ '() 'a]
                 (if (p? ([@ car 'a] xs))
                   ([@ cons 'a] ([@ car 'a] xs) 
                                (takewhile-mono p? ([@ cdr 'a] xs)))
                   [@ '() 'a]))))]
      takewhile-mono)))

    
        (check-expect ([@ takewhile int] even? [@ '() int])     [@ '() int])
        (check-expect ([@ takewhile int] even? '(1 4 2))        [@ '() int])
        (check-expect ([@ takewhile int] even? '(2 4 3))        '(2 4))
        (check-expect ([@ takewhile int] even? '(2 4 6 7 8 10)) '(2 4 6))

