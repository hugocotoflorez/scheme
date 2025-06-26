(use-modules (srfi srfi-1))

; list (merge list list)
; get the result of join and order two ordered lists
(define (merge l1 l2)
    (if (null? l1)
        (if (null? l2)
            (list)
            l2
        )
        (if (null? l2)
            l1
            (if (> (car l1) (car l2))
                (cons (car l1) (merge (cdr l1) l2))
                (cons (car l2) (merge (cdr l2) l1))
            )
        )
    )
)

(display (merge
           (list 9 7 4 3 1)
           (list 2 0))
)
