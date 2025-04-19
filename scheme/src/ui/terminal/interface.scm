;;; interface.scm -- Terminal interface for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code ui terminal interface)
  #:use-module (cuisine-code core kitchen kitchen)
  #:use-module (cuisine-code ui ascii-art ingredients)
  #:export (run-terminal-interface))

;; Run the terminal interface
(define (run-terminal-interface)
  (let ((kitchen (make-kitchen)))
    (display "Welcome to Cuisine Code Terminal Interface\n")
    (display "Type 'help' for commands, 'quit' to exit\n\n")
    
    (let loop ()
      (display "kitchen> ")
      (let ((input (read-line)))
        (cond
         ((eof-object? input)
          (display "\nGoodbye!\n"))
         
         ((string=? input "quit")
          (display "Goodbye!\n"))
         
         ((string=? input "help")
          (display-help)
          (loop))
         
         ((string=? input "stack")
          (display-stack kitchen)
          (loop))
         
         ((string-prefix? "push " input)
          (let ((ingredient (substring input 5)))
            (kitchen 'push ingredient)
            (display "Pushed: ")
            (display ingredient)
            (newline)
            (loop)))
         
         ((string=? input "pop")
          (let ((item (kitchen 'pop)))
            (display "Popped: ")
            (display item)
            (newline)
            (loop)))
         
         ((string=? input "swap")
          (kitchen 'swap)
          (display "Swapped top two items\n")
          (loop))
         
         ((string=? input "dup")
          (kitchen 'dup)
          (display "Duplicated top item\n")
          (loop))
         
         ((string-prefix? "transform " input)
          (let* ((rest (substring input 10))
                 (space-pos (string-index rest #\space))
                 (transform (if space-pos
                               (substring rest 0 space-pos)
                               rest))
                 (params (if space-pos
                            (read-from-string (substring rest (+ space-pos 1)))
                            '())))
            (kitchen 'transform (string->symbol transform) params)
            (display "Applied transformation: ")
            (display transform)
            (newline)
            (loop)))
         
         (else
          (display "Unknown command: ")
          (display input)
          (newline)
          (loop)))))))

;; Display help information
(define (display-help)
  (display "\nAvailable commands:\n")
  (display "  help              - Display this help\n")
  (display "  stack             - Display the current stack\n")
  (display "  push <ingredient> - Push an ingredient onto the stack\n")
  (display "  pop               - Remove the top item from the stack\n")
  (display "  swap              - Swap the top two items on the stack\n")
  (display "  dup               - Duplicate the top item on the stack\n")
  (display "  transform <name>  - Apply a transformation\n")
  (display "  quit              - Exit the program\n\n"))

;; Display the stack with ASCII art
(define (display-stack kitchen)
  (let ((items (kitchen 'stack)))
    (if (null? items)
        (display "Stack is empty\n")
        (begin
          (display "Stack (top to bottom):\n")
          (for-each
           (lambda (item)
             (display "-----------------------\n")
             (let ((art (ingredient->ascii item)))
               (for-each (lambda (line) 
                           (display line) 
                           (newline)) 
                         art))
             (display item)
             (newline))
           items)))))

;; Helper function to read from a string
(define (read-from-string str)
  (call-with-input-string str read))
