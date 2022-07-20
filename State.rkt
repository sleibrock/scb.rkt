#lang racket/base

#|
Hash.rkt - a simple hash macro library
|#


(provide State:get
         State:update
         State:update-else
         State:append
         )


;; Provide an easy way of accessing a hash key
;; Provide an error value if no key exists in hash
(define-syntax State:get
 (syntax-rules ()
  [(get-key-or H) (raise-syntax-error "State:get: no key provided!")]
  [(get-key-or H K)
   (if (hash-has-key? H K)
       (hash-ref H K)
       (error (format "State:get: key '~a' does not exist" K)))]
  [(get-key-or H K E)
    (cond
     [(hash-has-key? H K) (hash-ref H K)]
     [else E])]))


;; Provide a simpler use case for updating keys in a hash
;; Simply checks if key exists then uses the proper method
(define-syntax-rule (State:update H K V)
  (if (hash-has-key? H K)
      (hash-update H K (λ (_) V))
      (hash-set H K V)))


;; The other side of updating a hash
;; If the hash has a key in place, use the new value Y
;; If the value does not exist, use the new value N
(define-syntax-rule (State:update-else H K Y N)
  (if (hash-has-key? H K)
      (hash-update H K (λ (_) Y))
      (hash-set H K N)))


;; Append an element to a list inside a hash
;; If the original K->V is not a list, it is
;; then converted into a list with the new value
;; at the head, and the old data at the back
(define-syntax-rule (State:append H K V)
  (if (hash-has-key? H K)
      (hash-update H K
                   (λ (old)
                     (if (list? old)
                         (cons V old)
                         (list V old))))
      (hash-set H K (list V))))


(define-syntax-rule (State:keys H)
  (hash-keys H))


(define-syntax-rule (State:values H)
  (hash-values H))


;; Clear out a hash map entirely, by creating a new one
(define-syntax-rule (State:clear H)
  (make-immutable-hash '()))


; end
