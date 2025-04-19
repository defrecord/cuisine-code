;;; test-framework.scm -- Simple test framework for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code tests test-framework)
  #:export (define-test
            define-test-suite
            run-test
            run-test-suite
            assert-equal
            assert-true
            assert-false
            assert-error))

;; Test registry
(define *test-registry* '())

;; Define a test case
(define-macro (define-test name . body)
  `(begin
     (set! *test-registry* 
           (assoc-set! *test-registry* 
                       ',name
                       (lambda () ,@body)))
     ',name))

;; Define a test suite
(define-macro (define-test-suite name . tests)
  `(begin
     (set! *test-registry*
           (assoc-set! *test-registry*
                       ',name
                       (list ,@(map (lambda (test) `(quote ,test)) tests))))
     ',name))

;; Run a specific test
(define (run-test name)
  (let ((test-fn (assoc-ref *test-registry* name)))
    (if test-fn
        (if (procedure? test-fn)
            (begin
              (format #t "Running test: ~a\n" name)
              (let ((result (with-exception-handler
                             (lambda (exn)
                               `(failure ,exn))
                             (lambda ()
                               (test-fn)
                               'success))))
                (format #t "Test ~a: ~a\n" 
                        name 
                        (if (eq? result 'success) "PASSED" "FAILED"))
                (eq? result 'success)))
            (error "Not a test procedure" name))
        (error "Test not found" name))))

;; Run a test suite
(define (run-test-suite name)
  (let ((suite (assoc-ref *test-registry* name)))
    (if suite
        (if (list? suite)
            (begin
              (format #t "Running test suite: ~a\n" name)
              (let ((results (map run-test suite)))
                (let ((passed (count identity results))
                      (total (length results)))
                  (format #t "Suite ~a: ~a/~a tests passed\n" 
                          name passed total)
                  (= passed total))))
            (error "Not a test suite" name))
        (error "Test suite not found" name))))

;; Assertion helpers
(define (assert-equal expected actual)
  (if (equal? expected actual)
      #t
      (error "Assertion failed" 
             `(expected ,expected but got ,actual))))

(define (assert-true expr)
  (if expr
      #t
      (error "Assertion failed" 
             `(expected true but got ,expr))))

(define (assert-false expr)
  (if expr
      (error "Assertion failed" 
             `(expected false but got ,expr))
      #t))

(define (assert-error thunk)
  (let ((result (with-exception-handler
                 (lambda (exn) 'error-caught)
                 thunk)))
    (if (eq? result 'error-caught)
        #t
        (error "Assertion failed" 
               `(expected an error but none was raised)))))
