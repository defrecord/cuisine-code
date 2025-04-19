;;; basic.scm -- Basic culinary transformations for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code core transformations basic)
  #:use-module (cuisine-code core ingredients ingredient)
  #:export (chop
            dice
            mince
            slice
            julienne
            brunoise
            combine
            fold
            whip
            knead))

;; Mechanical transformations

(define (chop ingredient style)
  "Chop an ingredient with the specified style."
  (let ((name (ingredient-name ingredient))
        (props (ingredient-properties ingredient)))
    (make-ingredient 
     (string-append "chopped-" name)
     (assoc-set! props 'preparation 
                 (string-append "chopped-" (symbol->string style))))))

(define (dice ingredient size)
  "Dice an ingredient into cubes of the specified size."
  (let ((name (ingredient-name ingredient))
        (props (ingredient-properties ingredient)))
    (make-ingredient 
     (string-append "diced-" name)
     (assoc-set! props 'preparation 
                 (string-append "diced-" (symbol->string size))))))

(define (mince ingredient)
  "Mince an ingredient very finely."
  (let ((name (ingredient-name ingredient))
        (props (ingredient-properties ingredient)))
    (make-ingredient 
     (string-append "minced-" name)
     (assoc-set! props 'preparation "minced"))))

(define (slice ingredient thickness)
  "Slice an ingredient with the specified thickness."
  (let ((name (ingredient-name ingredient))
        (props (ingredient-properties ingredient)))
    (make-ingredient 
     (string-append "sliced-" name)
     (assoc-set! props 'preparation 
                 (string-append "sliced-" (symbol->string thickness))))))

(define (julienne ingredient)
  "Cut an ingredient into thin matchstick-like strips."
  (let ((name (ingredient-name ingredient))
        (props (ingredient-properties ingredient)))
    (make-ingredient 
     (string-append "julienned-" name)
     (assoc-set! props 'preparation "julienned"))))

(define (brunoise ingredient)
  "Cut an ingredient into very small cubes."
  (let ((name (ingredient-name ingredient))
        (props (ingredient-properties ingredient)))
    (make-ingredient 
     (string-append "brunoise-" name)
     (assoc-set! props 'preparation "brunoise"))))

;; Mixing transformations

(define (combine ingredients)
  "Combine multiple ingredients together."
  (let ((names (map ingredient-name ingredients))
        (all-props (map ingredient-properties ingredients)))
    (make-ingredient 
     (string-join names "-")
     (list (cons 'components ingredients)
           (cons 'preparation "combined")))))

(define (fold ingredients)
  "Gently fold ingredients together, preserving texture."
  (let ((names (map ingredient-name ingredients))
        (all-props (map ingredient-properties ingredients)))
    (make-ingredient 
     (string-join names "-folded")
     (list (cons 'components ingredients)
           (cons 'preparation "folded")))))

(define (whip ingredient)
  "Whip an ingredient to incorporate air."
  (let ((name (ingredient-name ingredient))
        (props (ingredient-properties ingredient)))
    (make-ingredient 
     (string-append "whipped-" name)
     (assoc-set! props 'preparation "whipped"))))

(define (knead ingredient duration)
  "Knead an ingredient for the specified duration."
  (let ((name (ingredient-name ingredient))
        (props (ingredient-properties ingredient)))
    (make-ingredient 
     (string-append "kneaded-" name)
     (assoc-set! (assoc-set! props 'preparation "kneaded")
                'duration duration))))
