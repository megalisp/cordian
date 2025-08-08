#lang racket


(require
  racket/base
  racket-cord
  racket/date)

(require
  "rson.rkt"
  "helpers.rkt"
  "context.rkt"
  "debug.rkt")

(provide start-event-logger on-event)

;; Catch-all event logger for diagnostics

;; Stub for channel name lookup (replace with real implementation)
(define (channel-name-by-id client channel-id)
  (string-append "#channel-" (if channel-id (format "~a" channel-id) "unknown")))

;; DSL for event summary rules
(define event-summary-rules
  (list
   (list 'raw-ready
     "user: ~a#~a id:~a"
     (lambda (client payload)
       (list (rget-in payload 'user 'username)
         (rget-in payload 'user 'discriminator)
         (rget-in payload 'user 'id))))
   (list 'raw-message-create
     "author: ~a#~a content: ~s"
     (lambda (client payload)
       (list (rget-in payload 'author 'username)
         (rget-in payload 'author 'discriminator)
         (rget payload 'content))))
   (list 'raw-message-update
     "id: ~a content: ~s"
     (lambda (client payload)
       (list (rget payload 'id)
         (rget payload 'content))))
   (list 'raw-message-delete
     "id: ~a channel: ~a (~a)"
     (lambda (client payload)
       (let* ([id (rget payload 'id)]
      [channel-id (rget payload 'channel_id)]
      [channel-name (channel-name-by-id client channel-id)])
     (list id channel-id channel-name))))
   (list 'raw-guild-create
     "guild: ~a (id:~a)"
     (lambda (client payload)
       (let ([id (rget payload 'id)]
         [name (rget payload 'name)])

     (list name id))))
   (list 'raw-guild-member-add
     "user: ~a#~a (id:~a)"
     (lambda (client payload)
       (list (rget-in payload 'user 'username)
         (rget-in payload 'user 'discriminator)
         (rget-in payload 'user 'id))))
   (list 'raw-thread-create
     "thread: ~a (id:~a) parent:~a"
     (lambda (client payload)
       (list (rget payload 'name)
         (rget payload 'id)
         (rget payload 'parent_id))))
   (list 'raw-thread-update
     "thread: ~a (id:~a) parent:~a"
     (lambda (client payload)
       (list (rget payload 'name)
         (rget payload 'id)
         (rget payload 'parent_id))))
   (list 'raw-thread-delete
     "thread: ~a (id:~a) parent:~a"
     (lambda (client payload)
       (list (rget payload 'name)
         (rget payload 'id)
         (rget payload 'parent_id))))
   (list 'raw-message-reaction-add
     "user: ~a emoji: ~a message: ~a"
     (lambda (client payload)
       (list (rget payload 'user_id)
         (rget-in payload 'emoji 'name)
         (rget payload 'message_id))))
   (list 'raw-message-reaction-remove
     "user: ~a emoji: ~a message: ~a"
     (lambda (client payload)
       (list (rget payload 'user_id)
         (rget-in payload 'emoji 'name)
         (rget payload 'message_id))))
   ))

;; Helper to pretty-print relevant event info using DSL
(define (event-summary evt payload #:client [client #f])
  (define rule (findf (lambda (r) (eq? (first r) evt)) event-summary-rules))
  (if rule
      (apply format (second rule) (apply (third rule) (list client payload)))
      (format "payload: ~s" (rselect payload (take (rkeys payload) 5)))))

(require "debug.rkt")

(define (start-event-logger client)
  (for ([evt (list 'raw-ready 'raw-message-create 'raw-message-update 'raw-message-delete
                   'raw-guild-create 'raw-guild-member-add
                   'raw-thread-create 'raw-thread-update 'raw-thread-delete
                   'raw-message-reaction-add 'raw-message-reaction-remove)])
    (on-event
     evt
     client
     (lambda (_ws-client client payload)
       (when (debug-event-logger-enabled)
         (debug-log "EVENT" "~a: ~a\n" evt (event-summary evt payload #:client client)))))))




