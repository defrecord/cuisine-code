;;; library-tests.scm -- Tests for recipe library
;;; Copyright (c) 2025 Aidan Pace

(use-modules (cuisine-code tests test-framework)
             (cuisine-code core recipes definition)
             (cuisine-code core recipes library))

;; Test library creation
(define-test test-library-creation
  (let ((library (make-recipe-library)))
    (assert-equal '() (library 'list))
    (assert-equal '() (library 'list-by-category "Basics"))
    (assert-equal '() (library 'list-by-difficulty 'beginner))))

;; Test adding recipes
(define-test test-add-recipe
  (let ((library (make-recipe-library))
        (recipe1 (make-recipe 
                  "Herb Butter"
                  "Basics"
                  'beginner
                  "Simple herb-infused butter"
                  '()
                  '()
                  '()))
        (recipe2 (make-recipe 
                  "Béchamel Sauce"
                  "Sauces"
                  'intermediate
                  "Classic white sauce"
                  '()
                  '()
                  '())))
    
    (library 'add recipe1)
    (library 'add recipe2)
    
    (assert-equal '("Herb Butter" "Béchamel Sauce") (library 'list))))

;; Test getting recipes
(define-test test-get-recipe
  (let ((library (make-recipe-library))
        (recipe (make-recipe 
                 "Herb Butter"
                 "Basics"
                 'beginner
                 "Simple herb-infused butter"
                 '()
                 '()
                 '())))
    
    (library 'add recipe)
    
    (let ((retrieved (library 'get "Herb Butter")))
      (assert-equal "Herb Butter" (recipe-name retrieved))
      (assert-equal "Basics" (recipe-category retrieved))
      
      (assert-error (lambda () (library 'get "Nonexistent Recipe"))))))

;; Test filtering recipes
(define-test test-filter-recipes
  (let ((library (make-recipe-library)))
    ;; Add recipes
    (library 'add (make-recipe "Herb Butter" "Basics" 'beginner "" '() '() '()))
    (library 'add (make-recipe "Mirepoix" "Basics" 'beginner "" '() '() '()))
    (library 'add (make-recipe "Béchamel Sauce" "Sauces" 'intermediate "" '() '() '()))
    (library 'add (make-recipe "Hollandaise Sauce" "Sauces" 'advanced "" '() '() '()))
    
    ;; Filter by category
    (let ((basics (library 'list-by-category "Basics"))
          (sauces (library 'list-by-category "Sauces"))
          (desserts (library 'list-by-category "Desserts")))
      
      (assert-equal 2 (length basics))
      (assert-equal 2 (length sauces))
      (assert-equal 0 (length desserts))
      
      (assert-equal "Basics" (recipe-category (car basics))))
    
    ;; Filter by difficulty
    (let ((beginner (library 'list-by-difficulty 'beginner))
          (intermediate (library 'list-by-difficulty 'intermediate))
          (advanced (library 'list-by-difficulty 'advanced)))
      
      (assert-equal 2 (length beginner))
      (assert-equal 1 (length intermediate))
      (assert-equal 1 (length advanced))
      
      (assert-equal 'beginner (recipe-difficulty (car beginner))))))

;; Test standard recipes
(define-test test-standard-recipes
  (let ((library (standard-recipes)))
    (assert-true (> (length (library 'list)) 0))
    
    (let ((mirepoix (library 'get "Basic Mirepoix")))
      (assert-equal "Basic Mirepoix" (recipe-name mirepoix))
      (assert-equal "Basics" (recipe-category mirepoix))
      (assert-equal 'beginner (recipe-difficulty mirepoix)))))

;; Define test suite
(define-test-suite library-tests
  test-library-creation
  test-add-recipe
  test-get-recipe
  test-filter-recipes
  test-standard-recipes)
