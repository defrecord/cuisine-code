;;; ingredient.scm -- Ingredient data structure for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code core ingredients ingredient)
  #:export (make-ingredient
            ingredient-name
            ingredient-properties
            ingredient-property
            set-ingredient-property!))

;; Create a new ingredient
(define (make-ingredient name properties)
  (let ((data (cons name properties)))
    ;; Return a dispatch procedure
    (lambda (operation . args)
      (case operation
        ((name) (car data))
        ((properties) (cdr data))
        ((property) 
         (let ((prop (car args)))
           (assoc-ref (cdr data) prop)))
        ((set-property!)
         (let ((prop (car args))
               (value (cadr args)))
           (set-cdr! data 
                     (assoc-set! (cdr data) prop value))))
        (else (error "Unknown ingredient operation" operation))))))

;; Helper functions for ingredient operations
(define (ingredient-name ingredient)
  (ingredient 'name))

(define (ingredient-properties ingredient)
  (ingredient 'properties))

(define (ingredient-property ingredient prop)
  (ingredient 'property prop))

(define (set-ingredient-property! ingredient prop value)
  (ingredient 'set-property! prop value))
