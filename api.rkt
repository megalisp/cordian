#lang racket

(provide delete-message)

(require (rename-in racket-cord/http [delete-message delete-message-http]))
(require (only-in "context.rkt" current-discord-client))

;; Helper to delete a message from context

(define (delete-message ctx)
  (define client (current-discord-client))
  (define event ctx)
  (define channel-id (hash-ref event 'channel_id))
  (define message-id (hash-ref event 'id))
  (with-handlers ([exn:fail? (lambda (e)
                                 (printf "[ERROR: DELETE-MESSAGE] Failed to delete message: Exception: ~a\n" e))])
    (delete-message-http client channel-id message-id)))

