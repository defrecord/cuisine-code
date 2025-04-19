;;; kitchen.scm -- Main kitchen interface for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code core kitchen kitchen)
  #:use-module (cuisine-code core kitchen stack)
  #:use-module (cuisine-code core ingredients store)
  #:use-module (cuisine-code core transformations registry)
  #:export (make-kitchen))

;; Create a new kitchen environment
(define (make-kitchen)
  (let ((stack (make-stack))
        (pantry (make-ingredient-store))
        (transformations (make-transformation-registry)))
    
    ;; Return a dispatch procedure
    (lambda (command . args)
      (case command
        ;; Stack operations
        ((push) (stack-push stack (car args)))
        ((pop) (stack-pop stack))
        ((peek) (stack-peek stack))
        ((swap) (stack-swap stack))
        ((dup) (stack-dup stack))
        ((rot) (stack-rot stack))
        ((stack) (stack->list stack))
        
        ;; Pantry operations
        ((add-ingredient) (pantry 'add (car args) (cadr args)))
        ((get-ingredient) (pantry 'get (car args)))
        ((list-ingredients) (pantry 'list))
        
        ;; Transformation operations
        ((transform)
         (let* ((name (car args))
                (params (if (null? (cdr args)) '() (cadr args)))
                (transform-fn (transformations 'get name)))
           (if transform-fn
               (let ((ingredients (stack-pop stack)))
                 (stack-push stack (apply transform-fn ingredients params)))
               (error "Unknown transformation" name))))
        
        ((register-transformation)
         (transformations 'register (car args) (cadr args)))
        
        ((list-transformations)
         (transformations 'list))
        
        ;; Unknown command
        (else (error "Unknown kitchen command" command))))))
