;;; stack.scm -- Stack implementation for Cuisine Code kitchen
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code core kitchen stack)
  #:export (make-stack
            stack-push
            stack-pop
            stack-peek
            stack-swap
            stack-dup
            stack-rot
            stack-size
            stack->list))

;; Create a new stack
(define (make-stack)
  (let ((items '()))
    ;; Return a dispatch procedure
    (lambda (operation . args)
      (case operation
        ((push) (set! items (cons (car args) items)))
        ((pop) 
         (if (null? items)
             (error "Cannot pop from empty stack")
             (let ((top (car items)))
               (set! items (cdr items))
               top)))
        ((peek) 
         (if (null? items)
             (error "Cannot peek empty stack")
             (car items)))
        ((swap) 
         (if (< (length items) 2)
             (error "Need at least two items to swap")
             (set! items (cons (cadr items)
                               (cons (car items)
                                     (cddr items))))))
        ((dup) 
         (if (null? items)
             (error "Cannot duplicate from empty stack")
             (set! items (cons (car items) items))))
        ((rot) 
         (if (< (length items) 3)
             (error "Need at least three items to rotate")
             (set! items (cons (caddr items)
                               (cons (car items)
                                     (cons (cadr items)
                                           (cdddr items)))))))
        ((size) (length items))
        ((->list) items)
        (else (error "Unknown stack operation" operation))))))

;; Helper functions for stack operations
(define (stack-push stack item)
  (stack 'push item))

(define (stack-pop stack)
  (stack 'pop))

(define (stack-peek stack)
  (stack 'peek))

(define (stack-swap stack)
  (stack 'swap))

(define (stack-dup stack)
  (stack 'dup))

(define (stack-rot stack)
  (stack 'rot))

(define (stack-size stack)
  (stack 'size))

(define (stack->list stack)
  (stack '->list))
