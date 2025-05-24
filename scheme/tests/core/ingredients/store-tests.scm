;;; store-tests.scm -- Tests for ingredient store
;;; Copyright (c) 2025 Aidan Pace

(use-modules (cuisine-code tests test-framework)
             (cuisine-code core ingredients store)
             (cuisine-code core ingredients ingredient))

;; Test store creation
(define-test test-store-creation
  (let ((store (make-ingredient-store)))
    (assert-equal '() (store 'list))
    (assert-equal 0 (store 'count))))

;; Test adding ingredients
(define-test test-add-ingredient
  (let ((store (make-ingredient-store)))
    (store 'add "butter" '((state . solid) (fat . high)))
    (store 'add "herbs" '((type . fresh) (flavor . strong)))
    
    (assert-equal '("butter" "herbs") (store 'list))
    (assert-equal 2 (store 'count))))

;; Test getting ingredients
(define-test test-get-ingredient
  (let ((store (make-ingredient-store)))
    (store 'add "butter" '((state . solid) (fat . high)))
    (store 'add "herbs" '((type . fresh) (flavor . strong)))
    
    (let ((butter (store 'get "butter"))
          (herbs (store 'get "herbs")))
      (assert-equal "butter" (ingredient-name butter))
      (assert-equal 'solid (ingredient-property butter 'state))
      (assert-equal "herbs" (ingredient-name herbs))
      (assert-equal 'fresh (ingredient-property herbs 'type))
      
      (assert-error (lambda () (store 'get "garlic"))))))

;; Test removing ingredients
(define-test test-remove-ingredient
  (let ((store (make-ingredient-store)))
    (store 'add "butter" '((state . solid) (fat . high)))
    (store 'add "herbs" '((type . fresh) (flavor . strong)))
    
    (store 'remove "butter")
    (assert-equal '("herbs") (store 'list))
    (assert-equal 1 (store 'count))
    
    (assert-error (lambda () (store 'get "butter")))))

;; Define test suite
(define-test-suite store-tests
  test-store-creation
  test-add-ingredient
  test-get-ingredient
  test-remove-ingredient)
