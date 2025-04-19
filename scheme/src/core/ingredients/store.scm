;;; store.scm -- Ingredient storage for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code core ingredients store)
  #:use-module (cuisine-code core ingredients ingredient)
  #:export (make-ingredient-store))

;; Create a new ingredient store (pantry)
(define (make-ingredient-store)
  (let ((ingredients (make-hash-table)))
    ;; Return a dispatch procedure
    (lambda (operation . args)
      (case operation
        ((add)
         (let ((name (car args))
               (properties (cadr args)))
           (hash-table-set! ingredients name 
                           (make-ingredient name properties))))
        
        ((get)
         (let ((name (car args)))
           (or (hash-table-ref ingredients name #f)
               (error "Ingredient not found" name))))
        
        ((remove)
         (let ((name (car args)))
           (hash-table-delete! ingredients name)))
        
        ((list)
         (hash-table-keys ingredients))
        
        ((count)
         (hash-table-size ingredients))
        
        (else (error "Unknown ingredient store operation" operation))))))
