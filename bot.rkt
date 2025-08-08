
#lang racket

(require
  racket/base
  racket/contract
  racket/match
  racket/cmdline
  (only-in racket-cord make-client))

(require
  "event.rkt"
  "context.rkt"
  "command.rkt"
  "utils.rkt"
  "debug.rkt")

(provide bot debug-event-logger-enabled debug-responses-enabled)

;; Only the new, flexible bot macro with debug toggles


(define-syntax (bot stx)
  (syntax-case stx ()
    [(_ #:startup-message msgs #:token token #:intents intents #:debug-bot-token debug-bot-token #:debug-event-logger debug-event-logger #:debug-responses debug-responses)
     #`(let* ()
         (let ([msg-list (vector->list msgs)])
           (for ([msg (in-list (drop-right msg-list 1))])
             (printf "~a\n" msg))
           (when (pair? msg-list)
             (printf "~a\n" (last msg-list)))
           (when (pair? msg-list)
             (newline)))
         (when debug-bot-token
           (debug-log "BOT" "BOT_TOKEN: ~a" token))
         (debug-log "BOT" "Creating bot client...")
         (define client (make-client-wrapper token #:intents intents))
         (debug-log "BOT" "Bot client created.")
         (current-discord-client client)
         (debug-event-logger-enabled debug-event-logger)
         (debug-responses-enabled debug-responses)
         (when debug-event-logger (start-event-logger client))
         client)]
    [(_ #:token token #:intents intents #:debug-bot-token debug-bot-token #:debug-event-logger debug-event-logger #:debug-responses debug-responses)
     #`(bot #:startup-message #() #:token token #:intents intents #:debug-bot-token debug-bot-token #:debug-event-logger debug-event-logger #:debug-responses debug-responses)]
    [(_ #:token token #:intents intents #:debug-bot-token debug-bot-token #:debug-event-logger debug-event-logger)
     #`(bot #:startup-message #() #:token token #:intents intents #:debug-bot-token debug-bot-token #:debug-event-logger debug-event-logger #:debug-responses #t)]
    [(_ #:token token #:intents intents #:debug-bot-token debug-bot-token)
     #`(bot #:startup-message #() #:token token #:intents intents #:debug-bot-token debug-bot-token #:debug-event-logger #t #:debug-responses #t)]
    [(_ #:token token #:intents intents)
     #`(bot #:startup-message #() #:token token #:intents intents #:debug-bot-token #f #:debug-event-logger #t #:debug-responses #t)]))

(provide debug-event-logger-enabled debug-responses-enabled)


(define (make-client-wrapper token #:intents [intents null])
  (make-client token #:intents intents))

