;;; middleware.scm -- Web server middleware for Cuisine Code
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code server middleware)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (cuisine-code server metrics)
  #:export (make-logging-middleware
            make-timing-middleware
            make-cors-middleware
            make-auth-middleware))

;; Logging middleware
(define (make-logging-middleware)
  (lambda (next)
    (lambda (request body)
      (let ((start-time (current-time)))
        (format #t "[~a] ~a ~a~%"
                (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time)))
                (request-method request)
                (uri-path (request-uri request)))
        
        ;; Increment request counter
        (increment-counter request-counter
                          (list (symbol->string (request-method request))
                                (uri-path (request-uri request))
                                "pending"))
        
        ;; Process request
        (let-values (((response response-body) (next request body)))
          
          ;; Log response
          (let* ((end-time (current-time))
                 (duration (- end-time start-time))
                 (status-code (response-code response)))
            
            (format #t "[~a] ~a ~a ~a ~a ms~%"
                    (strftime "%Y-%m-%d %H:%M:%S" (localtime end-time))
                    (request-method request)
                    (uri-path (request-uri request))
                    status-code
                    (* duration 1000))
            
            ;; Update metrics
            (increment-counter request-counter
                              (list (symbol->string (request-method request))
                                    (uri-path (request-uri request))
                                    (number->string status-code)))
            
            (observe-histogram request-duration
                              (list (symbol->string (request-method request))
                                    (uri-path (request-uri request)))
                              duration))
          
          ;; Return the response
          (values response response-body))))))

;; Timing middleware for performance measurement
(define (make-timing-middleware)
  (lambda (next)
    (lambda (request body)
      (let ((start-time (current-time)))
        (let-values (((response response-body) (next request body)))
          (let* ((end-time (current-time))
                 (duration (- end-time start-time)))
            
            ;; Add timing header
            (let ((headers (response-headers response)))
              (values (build-response
                       #:version (response-version response)
                       #:code (response-code response)
                       #:reason-phrase (response-reason-phrase response)
                       #:headers (cons (cons 'x-response-time 
                                            (format #f "~a ms" (* duration 1000)))
                                      headers))
                     response-body))))))))
response-body)))))))

;; CORS middleware
(define (make-cors-middleware allowed-origins)
  (lambda (next)
    (lambda (request body)
      (let-values (((response response-body) (next request body)))
        (let* ((origin (request-header request 'origin))
               (is-allowed (or (equal? allowed-origins "*")
                              (and origin (member origin allowed-origins)))))
          (if is-allowed
              (values (build-response
                       #:version (response-version response)
                       #:code (response-code response)
                       #:reason-phrase (response-reason-phrase response)
                       #:headers (append (response-headers response)
                                        `((access-control-allow-origin . ,origin)
                                          (access-control-allow-methods . "GET, POST, PUT, DELETE, OPTIONS")
                                          (access-control-allow-headers . "Content-Type, Authorization")
                                          (access-control-max-age . "86400"))))
                     response-body)
              (values response response-body)))))))

;; Authentication middleware
(define (make-auth-middleware auth-function)
  (lambda (next)
    (lambda (request body)
      (let ((auth-header (request-header request 'authorization)))
        (if (not auth-header)
            ;; No authentication header
            (if (public-path? (uri-path (request-uri request)))
                ;; Public path, proceed without auth
                (next request body)
                ;; Protected path, return 401
                (values (build-response
                         #:code 401
                         #:headers '((content-type . (application/json))))
                       "{\"error\":{\"code\":\"unauthorized\",\"message\":\"Authentication required\"}}"))
            ;; Authentication header present, validate
            (let ((token (extract-token auth-header)))
              (if (not token)
                  ;; Invalid token format
                  (values (build-response
                           #:code 401
                           #:headers '((content-type . (application/json))))
                         "{\"error\":{\"code\":\"invalid_token\",\"message\":\"Invalid authentication token format\"}}")
                  ;; Validate token
                  (let ((user-info (auth-function token)))
                    (if user-info
                        ;; Valid token, add user info to request
                        (next (add-user-to-request request user-info) body)
                        ;; Invalid token
                        (values (build-response
                                 #:code 401
                                 #:headers '((content-type . (application/json))))
                               "{\"error\":{\"code\":\"invalid_token\",\"message\":\"Invalid or expired authentication token\"}}"))))))))))

;; Helper functions for auth middleware
(define (public-path? path)
  (or (string=? path "/health")
      (string=? path "/metrics")
      (string=? path "/api/auth/login")
      (string=? path "/api/auth/register")))

(define (extract-token auth-header)
  (let ((parts (string-split auth-header " ")))
    (if (and (= (length parts) 2)
             (string-ci=? (car parts) "bearer"))
        (cadr parts)
        #f)))

(define (add-user-to-request request user-info)
  (let ((props (request-properties request)))
    (request-set-properties! request (cons (cons 'user user-info) props))
    request))
