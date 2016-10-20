; | *empty
(define empty '())
; .end

; | *head
(define head
  (lambda (stack)
    (car stack)))
; .end

; | *tail
(define tail
  (lambda (stack)
    (cdr stack)))
; .end

; | *cons
(define cons'
  (lambda (val rest)
    (cons val rest)))
; .end

; | *reverse
(define reverse
  (lambda (stack)
    (let (reverse-helper
          (lambda (curr acc)
            (cond
             (nil? acc) acc
             #t (reverse-helper (tail curr)
                                (cons' (car curr) acc)))))
      (reverse-helper stack))))
; .end
