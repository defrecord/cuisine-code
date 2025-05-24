;;; metrics.scm -- Prometheus metrics for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code server metrics)
  #:use-module (web server)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (make-metrics-middleware
            register-metric
            increment-counter
            observe-histogram
            set-gauge))

;; Metrics registry
(define *metrics-registry* '())

;; Register a new metric
(define (register-metric type name help labels)
  (let ((metric (list (cons 'type type)
                      (cons 'name name)
                      (cons 'help help)
                      (cons 'labels labels)
                      (cons 'values (make-hash-table)))))
    (set! *metrics-registry* (cons metric *metrics-registry*))
    metric))

;; Increment a counter metric
(define (increment-counter metric label-values &optional (value 1))
  (let ((values (assoc-ref metric 'values))
        (key (string-join label-values "_")))
    (hash-table-set! values key
                    (+ (or (hash-table-ref values key 0) 0) value))))

;; Observe a value in a histogram metric
(define (observe-histogram metric label-values value)
  (let ((values (assoc-ref metric 'values))
        (key (string-join label-values "_")))
    (let ((buckets (or (hash-table-ref values key #f)
                       (let ((new-buckets (make-default-buckets)))
                         (hash-table-set! values key new-buckets)
                         new-buckets))))
      (observe-bucket buckets value))))

;; Set a gauge metric value
(define (set-gauge metric label-values value)
  (let ((values (assoc-ref metric 'values))
        (key (string-join label-values "_")))
    (hash-table-set! values key value)))

;; Helper function to create default histogram buckets
(define (make-default-buckets)
  (list (cons 'buckets '(0.005 0.01 0.025 0.05 0.1 0.25 0.5 1 2.5 5 10))
        (cons 'counts (make-hash-table))
        (cons 'sum 0)
        (cons 'count 0)))

;; Helper function to observe a value in histogram buckets
(define (observe-bucket buckets value)
  (let ((bucket-limits (assoc-ref buckets 'buckets))
        (counts (assoc-ref buckets 'counts)))
    (for-each (lambda (limit)
                (when (<= value limit)
                  (hash-table-set! counts (number->string limit)
                                  (+ (hash-table-ref counts (number->string limit) 0) 1))))
              bucket-limits)
    ;; Update sum and count
    (set-cdr! (assoc 'sum buckets) (+ (assoc-ref buckets 'sum) value))
    (set-cdr! (assoc 'count buckets) (+ (assoc-ref buckets 'count) 1))))

;; Format metrics in Prometheus exposition format
(define (format-metrics)
  (define (format-metric metric)
    (let ((type (assoc-ref metric 'type))
          (name (assoc-ref metric 'name))
          (help (assoc-ref metric 'help))
          (labels (assoc-ref metric 'labels))
          (values (assoc-ref metric 'values)))
      (let ((output (string-append "# HELP " name " " help "\n"
                                  "# TYPE " name " " (symbol->string type) "\n")))
        (hash-table-for-each
         values
         (lambda (label-key value)
           (let ((label-values (string-split label-key "_")))
             (cond
              ((eq? type 'counter)
               (set! output (string-append output
                                         (format-labels name labels label-values)
                                         " " (number->string value) "\n")))
              ((eq? type 'gauge)
               (set! output (string-append output
                                         (format-labels name labels label-values)
                                         " " (number->string value) "\n")))
              ((eq? type 'histogram)
               (for-each
                (lambda (bucket-limit)
                  (let ((count (hash-table-ref (assoc-ref value 'counts) 
                                             (number->string bucket-limit)
                                             0)))
                    (set! output (string-append output
                                              (format-labels 
                                               (string-append name "_bucket")
                                               (append labels '("le"))
                                               (append label-values (list (number->string bucket-limit))))
                                              " " (number->string count) "\n"))))
                (assoc-ref value 'buckets))
               ;; Add sum and count
               (set! output (string-append output
                                         (format-labels 
                                          (string-append name "_sum")
                                          labels label-values)
                                         " " (number->string (assoc-ref value 'sum)) "\n"))
               (set! output (string-append output
                                         (format-labels 
                                          (string-append name "_count")
                                          labels label-values)
                                         " " (number->string (assoc-ref value 'count)) "\n")))))))
        output)))
  
  (string-join (map format-metric *metrics-registry*) ""))

;; Format labels for Prometheus output
(define (format-labels name labels values)
  (if (null? labels)
      name
      (string-append name "{"
                    (string-join
                     (map (lambda (label value)
                            (string-append (symbol->string label) "=\"" value "\""))
                          labels values)
                     ",")
                    "}")))

;; Create metrics middleware for the web server
(define (make-metrics-middleware)
  (lambda (next)
    (lambda (request body)
      (if (equal? (uri-path (request-uri request)) "/metrics")
          (values (build-response #:code 200
                                 #:headers '((content-type . (text/plain))))
                 (format-metrics))
          (next request body)))))

;; Initialize default metrics
(define request-counter
  (register-metric 'counter "http_requests_total"
                  "Total number of HTTP requests"
                  '(method path status_code)))

(define request-duration
  (register-metric 'histogram "http_request_duration_seconds"
                  "HTTP request duration in seconds"
                  '(method path)))

(define active_connections
  (register-metric 'gauge "http_active_connections"
                  "Number of active HTTP connections"
                  '()))

;; Set initial active connections to 0
(set-gauge active_connections '() 0)
