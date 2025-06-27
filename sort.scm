(use-modules (srfi srfi-1))

; list (merge list list)
; get the result of join and sort two sorted lists
(define (merge l1 l2)
    (if (null? l1)
        (if (null? l2)
            (list)
            l2
        )
        (if (null? l2)
            l1
            (if (< (car l1) (car l2))
                (cons (car l1) (merge (cdr l1) l2))
                (cons (car l2) (merge (cdr l2) l1))
            )
        )
    )
)

; list (mergesort list)
; get the result of sort a lists
(define (mergesort l)
  (if (null? l)
    (list)
    (if (= (length l) 1)
      l
      (merge_parts l (quotient (length l) 2))
    )
  )
)

; list (merge_parts list n)
; split the list L at index N, sort each part and merge them
(define (merge_parts l n)
  (if (zero? n)
    l
    (merge
      (mergesort (take_first l n))
      (mergesort (take_last l (- (length l) n)))
    )
  )
)

; list (take_first list n)
; get the first N elements of L
(define (take_first l n)
  (if (null? l)
    (list)
    (if (zero? n)
      (list)
      (append (list (car l)) (take_first (cdr l) (- n 1)))
    )
  )
)

; get the last N elements of L
(define (take_last l n)
  (drop l (- (length l) n))
)

; remove the first N elements of L and return new L
(define (drop l n)
  (if (zero? n)
    l
    (drop (cdr l) (- n 1))
  )

)

(display
  (mergesort (list 17 4 28 1 13 22 7 26 2 19 10 30 5 12 8 24 27 14 11 20))
)

