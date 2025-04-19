;;; registry.scm -- Transformation registry for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code core transformations registry)
  #:export (make-transformation-registry
            register-basic-transformations))

;; Create a new transformation registry
(define (make-transformation-registry)
  (let ((transformations (make-hash-table)))
    ;; Return a dispatch procedure
    (lambda (operation . args)
      (case operation
        ((register)
         (let ((name (car args))
               (function (cadr args)))
           (hash-table-set! transformations name function)))
        
        ((get)
         (let ((name (car args)))
           (or (hash-table-ref transformations name #f)
               (error "Transformation not found" name))))
        
        ((list)
         (hash-table-keys transformations))
        
        ((count)
         (hash-table-size transformations))
        
        (else (error "Unknown transformation registry operation" operation))))))

;; Register basic cooking transformations
(define (register-basic-transformations registry)
  ;; Mechanical transformations
  (registry 'register 'chop
           (lambda (ingredient . args)
             (let ((style (if (null? args) 'medium (car args))))
               (string-append (symbol->string style) "-chopped-" ingredient))))
  
  (registry 'register 'dice
           (lambda (ingredient . args)
             (let ((size (if (null? args) 'medium (car args))))
               (string-append (symbol->string size) "-diced-" ingredient))))
  
  (registry 'register 'mince
           (lambda (ingredient . args)
             (string-append "minced-" ingredient)))
  
  ;; Thermal transformations
  (registry 'register 'heat
           (lambda (ingredient . args)
             (let ((temp (if (null? args) 'medium (car args))))
               (string-append (symbol->string temp) "-heated-" ingredient))))
  
  (registry 'register 'saute
           (lambda (ingredient . args)
             (string-append "sauteed-" ingredient)))
  
  (registry 'register 'bake
           (lambda (ingredient . args)
             (let ((temp (if (null? args) 'medium (car args))))
               (string-append "baked-" ingredient))))
  
  ;; Mixing transformations
  (registry 'register 'combine
           (lambda (ingredients . args)
             (if (list? ingredients)
                 (string-append "combined-" (string-join ingredients "-and-"))
                 (error "Combine requires a list of ingredients"))))
  
  (registry 'register 'fold
           (lambda (ingredients . args)
             (if (list? ingredients)
                 (string-append "folded-" (string-join ingredients "-into-"))
                 (error "Fold requires a list of ingredients"))))
  
  ;; Return the registry
  registry)
