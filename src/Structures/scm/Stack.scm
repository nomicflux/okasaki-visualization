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
(define my-cons
  (lambda (val rest)
    (cons val rest)))
; .end

; | *reverse
(define reverse-helper
  (lambda (curr acc)
    (cond
     ((eq? curr empty) acc)
     (else (reverse-helper (tail curr)
                           (my-cons (car curr) acc))))))

(define my-reverse
  (lambda (stack)
      (reverse-helper stack empty)))
; .end
