;;; library.scm -- Recipe library for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code core recipes library)
  #:use-module (cuisine-code core recipes definition)
  #:export (make-recipe-library
            standard-recipes))

;; Create a new recipe library
(define (make-recipe-library)
  (let ((recipes (make-hash-table)))
    ;; Return a dispatch procedure
    (lambda (operation . args)
      (case operation
        ((add) 
         (let ((recipe (car args)))
           (hash-table-set! recipes (recipe 'name) recipe)))
        
        ((get)
         (let ((name (car args)))
           (or (hash-table-ref recipes name #f)
               (error "Recipe not found" name))))
        
        ((remove)
         (let ((name (car args)))
           (hash-table-delete! recipes name)))
        
        ((list)
         (hash-table-keys recipes))
        
        ((list-by-category)
         (let ((category (car args))
               (matching '()))
           (hash-table-for-each
            recipes
            (lambda (name recipe)
              (if (equal? (recipe 'category) category)
                  (set! matching (cons recipe matching)))))
           matching))
        
        ((list-by-difficulty)
         (let ((difficulty (car args))
               (matching '()))
           (hash-table-for-each
            recipes
            (lambda (name recipe)
              (if (equal? (recipe 'difficulty) difficulty)
                  (set! matching (cons recipe matching)))))
           matching))
        
        (else (error "Unknown recipe library operation" operation))))))

;; Create a library with standard recipes
(define (standard-recipes)
  (let ((library (make-recipe-library)))
    ;; Add some basic recipes
    
    ;; Compound Butter
    (library 'add
             (make-recipe 
              "Herb Compound Butter"
              "Basics"
              'beginner
              "A simple herb-infused butter for enhancing dishes."
              '(("butter" 250 "g")
                ("herbs" 30 "g")
                ("garlic" 2 "cloves")
                ("salt" 5 "g"))
              '((push "butter")
                (transform 'soften '((temperature . 20)))
                (push "herbs")
                (transform 'chop '((style . 'fine)))
                (push "garlic")
                (transform 'mince)
                (push "salt")
                (transform 'combine)
                (transform 'shape '((form . 'log)))
                (transform 'chill '((duration . 120))))
              '((type . "compound-butter")
                (properties . ((state . 'solid)
                              (flavor . 'herb-garlic))))))
    
    ;; Basic Mirepoix
    (library 'add
             (make-recipe 
              "Basic Mirepoix"
              "Basics"
              'beginner
              "The aromatic flavor base of French cuisine."
              '(("onion" 1 "medium")
                ("carrot" 2 "medium")
                ("celery" 2 "stalks"))
              '((push "onion")
                (transform 'dice '((size . 'small)))
                (push "carrot")
                (transform 'dice '((size . 'small)))
                (push "celery")
                (transform 'dice '((size . 'small)))
                (transform 'combine)
                (transform 'sweat '((duration . 10))))
              '((type . "mirepoix")
                (properties . ((state . 'cooked)
                              (flavor . 'aromatic))))))
    
    ;; Return the library
    library))
