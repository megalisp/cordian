#lang racket


(require
  racket/base
  (only-in racket-cord/http create-message))

(require
  "context.rkt"
  "debug.rkt")

(provide respond reports)

;; report: prints a message to the terminal for command logic explanations
(define (reports msg . args)
  (apply debug-log "REPORTS" msg args))


(define (respond msg #:client [client #f] #:channel [channel-id #f] #:thread [thread-id #f])
  (define event (current-event))
  (define real-client (or client (current-discord-client)))
  (define real-channel (or channel-id (and event (hash-ref event 'channel_id #f))))
  (when (debug-responses-enabled)
  (debug-log "RESPOND" "event: ~a" event)
  (debug-log "RESPOND" "real-client: ~a" real-client)
  (debug-log "RESPOND" "real-channel: ~a" real-channel)
  (debug-log "RESPOND" "msg: ~a" msg)
  (debug-log "RESPOND" "thread-id: ~a" thread-id))
  (cond
    [thread-id (create-message real-client real-channel msg #:thread-id thread-id)]
    [else (create-message real-client real-channel msg)]))

