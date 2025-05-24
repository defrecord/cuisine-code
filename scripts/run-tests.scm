#!/usr/bin/env guile
!#

;;; run-tests.scm -- Run all tests for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

;; Add project root to load path
(add-to-load-path ".")

(use-modules (cuisine-code tests test-framework))

;; Load all test modules
(display "Loading test modules...\n")

;; Core kitchen tests
(load "scheme/tests/core/kitchen/stack-tests.scm")
(load "scheme/tests/core/kitchen/kitchen-tests.scm")

;; Core ingredient tests
(load "scheme/tests/core/ingredients/ingredient-tests.scm")
(load "scheme/tests/core/ingredients/store-tests.scm")

;; Core transformation tests
(load "scheme/tests/core/transformations/registry-tests.scm")

;; Core recipe tests
(load "scheme/tests/core/recipes/definition-tests.scm")
(load "scheme/tests/core/recipes/execution-tests.scm")
(load "scheme/tests/core/recipes/library-tests.scm")

;; Define main test suite
(define-test-suite all-tests
  stack-tests
  kitchen-tests
  ingredient-tests
  store-tests
  registry-tests
  definition-tests
  execution-tests
  library-tests)

;; Run all tests
(define (execute-test-suite)
  (display "\nRunning all tests...\n")
  (let ((result (run-test-suite 'all-tests)))
    (if result
        (begin
          (display "\nAll tests passed!\n")
          (exit 0))
        (begin
          (display "\nSome tests failed!\n")
          (exit 1)))))

;; Run tests if this script is the main program
(if (eq? (current-module) (resolve-module '(guile-user)))
    (execute-test-suite))
