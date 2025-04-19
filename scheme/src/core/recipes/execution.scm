;;; execution.scm -- Recipe execution for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code core recipes execution)
  #:use-module (cuisine-code core kitchen kitchen)
  #:use-module (cuisine-code core recipes definition)
  #:export (execute-recipe
            step-recipe))

;; Execute a complete recipe
(define (execute-recipe kitchen recipe)
  (let ((steps (recipe-steps recipe)))
    ;; Execute each step in sequence
    (for-each (lambda (step)
                (apply-step kitchen step))
              steps)
    ;; Return the top item on the stack as the result
    (kitchen 'peek)))

;; Execute a single step of a recipe
(define (step-recipe kitchen recipe step-index)
  (let ((steps (recipe-steps recipe)))
    (if (< step-index (length steps))
        (apply-step kitchen (list-ref steps step-index))
        (error "Step index out of bounds" step-index))))

;; Apply a single recipe step to the kitchen
(define (apply-step kitchen step)
  (let ((operation (car step))
        (args (cdr step)))
    (case operation
      ((push) (apply kitchen 'push args))
      ((pop) (kitchen 'pop))
      ((swap) (kitchen 'swap))
      ((dup) (kitchen 'dup))
      ((rot) (kitchen 'rot))
      ((transform) (apply kitchen 'transform args))
      (else (error "Unknown recipe step operation" operation)))))
