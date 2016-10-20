; | *DataStructure Stack
(deftype Stack [car cdr])
; .end

; | *empty
(def empty nil)
; .end

; | *head
(defn head [stack]
  (.car stack))
; .end

; | *tail
(defn tail [stack]
  (.cdr stack))
; .end

; | *cons
(defn cons' [x rest]
  (Stack. x rest))
; .end

; | *reverse
(defn reverse [x]
  (loop [curr x
         acc empty]
    (cond (= curr empty)
          acc
          :else
          (recur (tail curr)
                 (cons' (head curr) acc)))))
; .end
