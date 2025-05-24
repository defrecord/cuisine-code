;;; registry-tests.scm -- Tests for transformation registry
;;; Copyright (c) 2025 Aidan Pace

(use-modules (cuisine-code tests test-framework)
             (cuisine-code core transformations registry))

;; Test registry creation
(define-test test-registry-creation
  (let ((registry (make-transformation-registry)))
    (assert-equal '() (registry 'list))
    (assert-equal 0 (registry 'count))))

;; Test registering transformations
(define-test test-register-transformation
  (let ((registry (make-transformation-registry)))
    (registry 'register 'chop
             (lambda (ingredient . args)
               (string-append "chopped-" ingredient)))
    
    (registry 'register 'dice
             (lambda (ingredient . args)
               (string-append "diced-" ingredient)))
    
    (assert-equal '(chop dice) (registry 'list))
    (assert-equal 2 (registry 'count))))

;; Test getting transformations
(define-test test-get-transformation
  (let ((registry (make-transformation-registry)))
    (registry 'register 'chop
             (lambda (ingredient . args)
               (string-append "chopped-" ingredient)))
    
    (let ((chop-fn (registry 'get 'chop)))
      (assert-true (procedure? chop-fn))
      (assert-equal "chopped-carrot" (chop-fn "carrot"))
      
      (assert-error (lambda () (registry 'get 'dice))))))

;; Test basic transformations registration
(define-test test-basic-transformations
  (let ((registry (make-transformation-registry)))
    (register-basic-transformations registry)
    
    (assert-true (> (registry 'count) 0))
    
    (let ((chop-fn (registry 'get 'chop))
          (dice-fn (registry 'get 'dice))
          (saute-fn (registry 'get 'saute))
          (combine-fn (registry 'get 'combine)))
      
      (assert-equal "medium-chopped-carrot" (chop-fn "carrot"))
      (assert-equal "fine-chopped-carrot" (chop-fn "carrot" 'fine))
      
      (assert-equal "medium-diced-onion" (dice-fn "onion"))
      (assert-equal "small-diced-onion" (dice-fn "onion" 'small))
      
      (assert-equal "sauteed-mushroom" (saute-fn "mushroom"))
      
      (assert-equal "combined-butter-and-herbs" 
                    (combine-fn '("butter" "herbs")))
      
      (assert-error (lambda () (combine-fn "butter"))))))

;; Define test suite
(define-test-suite registry-tests
  test-registry-creation
  test-register-transformation
  test-get-transformation
  test-basic-transformations)
