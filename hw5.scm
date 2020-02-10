
; Innocent Kironji
; CMSC 331-02
;====================================================================================================================

; Question 3
; Gives the volume and surface area of a cone given its height and radius
(define cone-facts
  (lambda (r h)
    (list  (* (/ 1 3) 3.14 r r h) 
	   (+ (* 3.14 r r) (* 3.14 r (sqrt (+ (* r r) (* h h))))) 
    ) 
  )
)
;====================================================================================================================

; Question 4
; Moves the first two entries of a list to the back in reverse order
(define double-rotate
  (lambda (li)
    (append (cdr(cdr li)) (list (car(cdr li)) (car li)) )
   )
 )
;====================================================================================================================

; Question 5
; Returns a selected portion from a list of numbers
(define slice
  (lambda (li start end)
    (slice-kernel (slice-helper li start) end '() )
  )
)

(define slice-kernel     ; Majority of the slicing is done in this function
  (lambda (li end result)
    (cond
     ( (null? li) result)
     ( (equal? (car li) end) (append result (list (car li))) )
     (else (slice-kernel (cdr li) end (append result (list (car li))) ) )
    )
  )
)

(define slice-helper ; Cuts the list until the start of the slice is reached
  (lambda (li start)
    (cond
     ( (null? li) li)
     ( (equal? (car li) start) (cdr li) )
     ( (> (car li) start) li)
     (else (slice-helper (cdr li) start) )
    )
  )
)
;====================================================================================================================

; Question 6
; Removes the first instance of a given target from a list
(define remove-first
  (lambda (li target)
    (append (remove-first-k1 li target '() ) (remove-first-k2 li target '() ) )
  )
)
(define remove-first-k1   ;Constructs the portion of the list before the removed entry
  (lambda (li target prepender)
    (cond
     ( (null? li) prepender )
     ( (equal? (car li) target) prepender)
     (else (remove-first-k1 (cdr li) target (append prepender (list (car li)) ) ) )
    )
  )
)
(define remove-first-k2    ;Constructs the portion of the list follwing the removed entry
  (lambda (li target appender)
    (cond
     ( (null? li) appender)
     ( (equal? (car li) target) (append (cdr li) appender) )
     (else (remove-first-k2 (cdr li) target appender) )
    )
  )
)

;====================================================================================================================

; Question 7
; Given a list returns perfect squares from that list
(define perfect-squares
  (lambda (nums)
    (map * (filter integer? (map sqrt nums))  (filter integer? (map sqrt nums)) )
  )
)

;====================================================================================================================

; Question 8
; Calculates standard deviation of a list of numbers
(define sd
  (lambda (nums)
    (define average (avg nums)) ;Calculating average beforehand
    (sqrt ( /
      (apply + (map (lambda(x) 
	       (* (- x average) (- x average) ) ) nums)
		   
      )
      (- (length nums) 1) )
    )
  )
)

; Helper function that calculates the average of a list of numbers
(define avg 
  (lambda (li)
    ( / (apply + li) (length li) )
  )
)