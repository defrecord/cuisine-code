;;; websocket.scm -- WebSocket server for collaborative cooking
;;; Copyright (c) 2025 Aidan Pace

(define-module (cuisine-code server websocket)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (json)
  #:export (start-websocket-server
            broadcast-message
            send-message-to-user
            register-client))

;; Client connections
(define *clients* '())

;; Client types
(define-record-type <client>
  (make-client id user-id socket room-id)
  client?
  (id client-id)          ; Connection ID
  (user-id client-user-id) ; User ID
  (socket client-socket)   ; WebSocket connection
  (room-id client-room-id client-set-room-id!)) ; Room ID (for collaborative sessions)

;; Start WebSocket server
(define (start-websocket-server port)
  (let ((server (make-websocket-server port)))
    (server 'start)
    server))

;; Create WebSocket server
(define (make-websocket-server port)
  (let ((server (make-http-server))
        (running #f))
    
    ;; Configure server to handle WebSocket connections
    (server 'handler (make-websocket-handler))
    
    ;; Server interface
    (lambda (command . args)
      (case command
        ((start)
         (set! running #t)
         (server 'listen port)
         (format #t "WebSocket server started on port ~a~%" port))
        
        ((stop)
         (set! running #f)
         (server 'close))
        
        ((status)
         (if running 'running 'stopped))
        
        ((clients)
         *clients*)
        
        (else (error "Unknown WebSocket server command:" command))))))

;; WebSocket handler
(define (make-websocket-handler)
  (lambda (request body)
    (if (websocket-request? request)
        ;; Handle WebSocket upgrade
        (handle-websocket-upgrade request)
        ;; Regular HTTP request
        (values (build-response #:code 400)
                "Not a WebSocket request"))))

;; Check if request is a WebSocket upgrade
(define (websocket-request? request)
  (and (equal? (request-method request) 'GET)
       (equal? (request-header request 'upgrade) "websocket")
       (equal? (request-header request 'connection) "Upgrade")))

;; Handle WebSocket upgrade
(define (handle-websocket-upgrade request)
  ;; This is a simplified placeholder - a real implementation
  ;; would perform the WebSocket handshake and establish connection
  (format #t "Received WebSocket upgrade request~%")
  
  ;; Extract user authentication
  (let ((auth-token (request-header request 'authorization)))
    (if auth-token
        (let ((user-id (validate-auth-token auth-token)))
          (if user-id
              ;; Create new client and connection
              (let ((client-id (generate-unique-id))
                    (socket (create-websocket-connection)))
                (register-client client-id user-id socket #f)
                (format #t "WebSocket connection established for user ~a~%" user-id)
                
                ;; Return upgrade response
                (values (build-response
                         #:code 101
                         #:reason-phrase "Switching Protocols"
                         #:headers '((upgrade . "websocket")
                                    (connection . "Upgrade")))
                       #f))
              ;; Invalid auth token
              (values (build-response #:code 401)
                     "Unauthorized")))
        ;; No auth token
        (values (build-response #:code 401)
               "Unauthorized"))))

;; Register a client connection
(define (register-client client-id user-id socket room-id)
  (set! *clients*
        (cons (make-client client-id user-id socket room-id)
              *clients*)))

;; Validate authentication token
(define (validate-auth-token token)
  ;; This is a placeholder - in a real application, you would
  ;; validate the token and extract the user ID
  (if (string? token)
      (let ((parts (string-split token " ")))
        (if (and (= (length parts) 2)
                 (string-ci=? (car parts) "bearer"))
            (let ((jwt (cadr parts)))
              ;; Parse and validate JWT - this is simplified
              (if (> (string-length jwt) 10)
                  (string-append "user-" (substring jwt 0 6))
                  #f))
            #f))
      #f))

;; Generate a unique client ID
(define (generate-unique-id)
  (format #f "ws-~a" (random 1000000)))

;; Create WebSocket connection
(define (create-websocket-connection)
  ;; This is a placeholder - in a real implementation, you would
  ;; create a proper WebSocket connection object
  (format #t "Creating WebSocket connection~%")
  'websocket-connection)

;; Send message to a specific user
(define (send-message-to-user user-id message)
  (let ((user-clients (filter (lambda (client)
                               (equal? (client-user-id client) user-id))
                             *clients*)))
    (for-each (lambda (client)
                (send-message (client-socket client) message))
              user-clients)))

;; Broadcast message to room
(define (broadcast-message room-id message)
  (let ((room-clients (filter (lambda (client)
                               (equal? (client-room-id client) room-id))
                             *clients*)))
    (for-each (lambda (client)
                (send-message (client-socket client) message))
              room-clients)))

;; Send message to WebSocket
(define (send-message socket message)
  ;; This is a placeholder - in a real implementation, you would
  ;; send the message through the WebSocket
  (format #t "Sending message to socket: ~a~%" message))

;; WebSocket message handling
(define (handle-websocket-message client message)
  (let ((data (json-string->scm message)))
    (case (assoc-ref data 'type)
      ((join-room)
       (let ((room-id (assoc-ref data 'room_id)))
         (client-set-room-id! client room-id)
         (format #t "Client ~a joined room ~a~%"
                 (client-id client) room-id)
         (broadcast-message room-id
                           (scm->json-string
                            `((type . "user-joined")
                              (user_id . ,(client-user-id client)))))))
      
      ((leave-room)
       (let ((room-id (client-room-id client)))
         (client-set-room-id! client #f)
         (format #t "Client ~a left room ~a~%"
                 (client-id client) room-id)
         (broadcast-message room-id
                           (scm->json-string
                            `((type . "user-left")
                              (user_id . ,(client-user-id client)))))))
      
      ((kitchen-update)
       (let ((room-id (client-room-id client))
             (stack (assoc-ref data 'stack)))
         (format #t "Kitchen update in room ~a~%" room-id)
         (broadcast-message room-id
                           (scm->json-string
                            `((type . "kitchen-update")
                              (user_id . ,(client-user-id client))
                              (stack . ,stack))))))
      
      ((chat-message)
       (let ((room-id (client-room-id client))
             (text (assoc-ref data 'text)))
         (format #t "Chat message in room ~a: ~a~%"
                 room-id text)
         (broadcast-message room-id
                           (scm->json-string
                            `((type . "chat-message")
                              (user_id . ,(client-user-id client))
                              (text . ,text)
                              (timestamp . ,(current-time)))))))
      
      (else
       (format #t "Unknown message type: ~a~%"
               (assoc-ref data 'type))))))
