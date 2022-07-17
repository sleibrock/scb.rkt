#lang racket/base

#|
Hash.rkt - a simple hash macro library
|#


(provide all-defined-out)


;; Provide an easy way of accessing a hash key
;; Provide an error value if no key exists in hash
(define-syntax Hash:get
 (syntax-rules ()
  [(get-key-or H) (raise-syntax-error "Hash:get: no key provided!")]
  [(get-key-or H K)
   (if (hash-has-key? H K)
       (hash-ref H K)
       (error (format "Hash:get: key '~a' does not exist" K)))]
  [(get-key-or H K E)
    (cond
     [(hash-has-key? H K) (hash-ref H K)]
     [else E])]))


;; Provide a simpler use case for updating keys in a hash
;; Simply checks if key exists then uses the proper method
(define-syntax-rule (Hash:update H K V)
  (if (hash-has-key? H K)
      (hash-update H K (λ (_) V))
      (hash-set H K V)))


;; The other side of updating a hash
;; If the hash has a key in place, use the new value Y
;; If the value does not exist, use the new value N
(define-syntax-rule (Hash:update-else H K Y N)
  (if (hash-has-key? H K)
      (hash-update H K (λ (_) Y))
      (hash-set H K N)))


; end
