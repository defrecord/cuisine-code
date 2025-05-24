;;; execution-tests.scm -- Tests for recipe execution
;;; Copyright (c) 2025 Aidan Pace

(use-modules (cuisine-code tests test-framework)
             (cuisine-code core kitchen kitchen)
             (cuisine-code core recipes definition)
             (cuisine-code core recipes execution))

;; Utility to create a test kitchen
(define (create-test-kitchen)
  (let ((kitchen (make-kitchen)))
    ;; Register basic transformations
    (kitchen 'register-transformation 'chop
             (lambda (ingredient . args)
               (string-append "chopped-" ingredient)))
    
    (kitchen 'register-transformation 'mix
             (lambda (ingredients . args)
               (if (list? ingredients)
                   (string-append "mixed-" (string-join ingredients "-and-"))
                   ingredients)))
    
    kitchen))

;; Test complete recipe execution
(define-test test-execute-recipe
  (let ((kitchen (create-test-kitchen))
        (recipe (make-recipe 
                 "Herb Butter"
                 "Basics"
                 'beginner
                 "Simple herb-infused butter"
                 '(("butter" 250 "g") ("herbs" 30 "g"))
                 '((push "butter") 
                   (push "herbs") 
                   (transform 'chop) 
                   (transform 'mix))
                 '((type . "compound-butter")))))
    
    (let ((result (execute-recipe kitchen recipe)))
      (assert-equal "mixed-butter-and-chopped-herbs" result))))

;; Test step-by-step recipe execution
(define-test test-step-recipe
  (let ((kitchen (create-test-kitchen))
        (recipe (make-recipe 
                 "Herb Butter"
                 "Basics"
                 'beginner
                 "Simple herb-infused butter"
                 '(("butter" 250 "g") ("herbs" 30 "g"))
                 '((push "butter") 
                   (push "herbs") 
                   (transform 'chop) 
                   (transform 'mix))
                 '((type . "compound-butter")))))
    
    ;; Step 1: Push butter
    (step-recipe kitchen recipe 0)
    (assert-equal '("butter") (kitchen 'stack))
    
    ;; Step 2: Push herbs
    (step-recipe kitchen recipe 1)
    (assert-equal '("herbs" "butter") (kitchen 'stack))
    
    ;; Step 3: Chop herbs
    (step-recipe kitchen recipe 2)
    (assert-equal '("chopped-herbs" "butter") (kitchen 'stack))
    
    ;; Step 4: Mix ingredients
    (step-recipe kitchen recipe 3)
    (assert-equal '("mixed-butter-and-chopped-herbs") (kitchen 'stack))
    
    ;; Test out-of-bounds steps
    (assert-error (lambda () (step-recipe kitchen recipe 4)))))

;; Define test suite
(define-test-suite execution-tests
  test-execute-recipe
  test-step-recipe)
