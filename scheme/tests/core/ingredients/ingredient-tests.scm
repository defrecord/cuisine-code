;;; ingredient-tests.scm -- Tests for ingredient implementation
;;; Copyright (c) 2025 Aidan Pace

(use-modules (cuisine-code tests test-framework)
             (cuisine-code core ingredients ingredient))

;; Test ingredient creation
(define-test test-ingredient-creation
  (let ((ingredient (make-ingredient "butter" '((state . solid) (fat . high)))))
    (assert-equal "butter" (ingredient-name ingredient))
    (assert-equal '((state . solid) (fat . high)) (ingredient-properties ingredient))))

;; Test ingredient properties
(define-test test-ingredient-properties
  (let ((ingredient (make-ingredient "butter" '((state . solid) (fat . high)))))
    (assert-equal 'solid (ingredient-property ingredient 'state))
    (assert-equal 'high (ingredient-property ingredient 'fat))
    (assert-equal #f (ingredient-property ingredient 'flavor))))

;; Test setting ingredient properties
(define-test test-set-ingredient-property
  (let ((ingredient (make-ingredient "butter" '((state . solid) (fat . high)))))
    (set-ingredient-property! ingredient 'state 'melted)
    (assert-equal 'melted (ingredient-property ingredient 'state))
    (set-ingredient-property! ingredient 'flavor 'rich)
    (assert-equal 'rich (ingredient-property ingredient 'flavor))))

;; Define test suite
(define-test-suite ingredient-tests
  test-ingredient-creation
  test-ingredient-properties
  test-set-ingredient-property)
