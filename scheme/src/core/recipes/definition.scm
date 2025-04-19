;;; definition.scm -- Recipe definition for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code core recipes definition)
  #:export (make-recipe
            define-recipe
            recipe-name
            recipe-category
            recipe-difficulty
            recipe-description
            recipe-ingredients
            recipe-steps
            recipe-expected-result))

;; Create a new recipe
(define (make-recipe name category difficulty description ingredients steps expected-result)
  (let ((data (list (cons 'name name)
                    (cons 'category category)
                    (cons 'difficulty difficulty)
                    (cons 'description description)
                    (cons 'ingredients ingredients)
                    (cons 'steps steps)
                    (cons 'expected-result expected-result))))
    ;; Return a dispatch procedure
    (lambda (operation . args)
      (case operation
        ((name) (assoc-ref data 'name))
        ((category) (assoc-ref data 'category))
        ((difficulty) (assoc-ref data 'difficulty))
        ((description) (assoc-ref data 'description))
        ((ingredients) (assoc-ref data 'ingredients))
        ((steps) (assoc-ref data 'steps))
        ((expected-result) (assoc-ref data 'expected-result))
        (else (error "Unknown recipe operation" operation))))))

;; Macro for defining recipes
(define-syntax define-recipe
  (syntax-rules (:name :category :difficulty :description :ingredients :steps :expected-result)
    ((_ recipe-id
        :name name
        :category category
        :difficulty difficulty
        :description description
        :ingredients ingredients
        :steps steps
        :expected-result expected-result)
     (define recipe-id
       (make-recipe name
                    category
                    difficulty
                    description
                    ingredients
                    steps
                    expected-result)))))

;; Helper functions for recipe operations
(define (recipe-name recipe)
  (recipe 'name))

(define (recipe-category recipe)
  (recipe 'category))

(define (recipe-difficulty recipe)
  (recipe 'difficulty))

(define (recipe-description recipe)
  (recipe 'description))

(define (recipe-ingredients recipe)
  (recipe 'ingredients))

(define (recipe-steps recipe)
  (recipe 'steps))

(define (recipe-expected-result recipe)
  (recipe 'expected-result))
