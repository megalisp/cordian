
#lang racket

(provide current-event current-discord-client)

(define current-event (make-parameter #f))
(define current-discord-client (make-parameter #f))
