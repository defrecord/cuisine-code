;;; definition-tests.scm -- Tests for recipe definition
;;; Copyright (c) 2025 Aidan Pace

(use-modules (cuisine-code tests test-framework)
             (cuisine-code core recipes definition))

;; Test recipe creation
(define-test test-recipe-creation
  (let ((recipe (make-recipe 
                 "Herb Butter"
                 "Basics"
                 'beginner
                 "Simple herb-infused butter"
                 '(("butter" 250 "g") ("herbs" 30 "g"))
                 '((push "butter") (push "herbs") (transform 'combine))
                 '((type . "compound-butter")))))
    
    (assert-equal "Herb Butter" (recipe-name recipe))
    (assert-equal "Basics" (recipe-category recipe))
    (assert-equal 'beginner (recipe-difficulty recipe))
    (assert-equal "Simple herb-infused butter" (recipe-description recipe))
    (assert-equal '(("butter" 250 "g") ("herbs" 30 "g")) 
                  (recipe-ingredients recipe))
    (assert-equal '((push "butter") (push "herbs") (transform 'combine)) 
                  (recipe-steps recipe))
    (assert-equal '((type . "compound-butter")) 
                  (recipe-expected-result recipe))))

;; Test recipe definition macro
(define-test test-recipe-definition-macro
  (define-recipe test-recipe
    :name "Herb Butter"
    :category "Basics"
    :difficulty 'beginner
    :description "Simple herb-infused butter"
    :ingredients '(("butter" 250 "g") ("herbs" 30 "g"))
    :steps '((push "butter") (push "herbs") (transform 'combine))
    :expected-result '((type . "compound-butter")))
  
  (assert-equal "Herb Butter" (recipe-name test-recipe))
  (assert-equal "Basics" (recipe-category test-recipe))
  (assert-equal 'beginner (recipe-difficulty test-recipe)))

;; Define test suite
(define-test-suite definition-tests
  test-recipe-creation
  test-recipe-definition-macro)
