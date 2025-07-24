#lang racket/base

(require racket/random)

(provide
 make-condvar
 condvar?
 condvar-signal
 condvar-broadcast
 condvar-wait-evt
 condvar-wait)

(struct condvar ([waiters #:mutable]))

(define (make-condvar)
  (condvar null))

(define (condvar-signal cvar)
  (void (signal-next-waiter! cvar)))

(define (condvar-broadcast cvar)
  (when (signal-next-waiter! cvar)
    (void (condvar-broadcast cvar))))

(define (condvar-wait-evt cvar mutex)
  (nack-guard-evt
   (lambda (nack)
     (define waiter (make-semaphore))
     (add-condvar-waiter! cvar waiter)
     (thread
      (lambda ()
        (sync nack)
        (semaphore-wait mutex)
        (remove-condvar-waiter! cvar waiter)
        (semaphore-post mutex)))
     (semaphore-post mutex)
     (replace-evt waiter (λ (_) mutex)))))

(define (condvar-wait cvar mutex)
  (sync/enable-break (condvar-wait-evt cvar mutex)))

(define (add-condvar-waiter! cvar waiter)
  (define waiters (condvar-waiters cvar))
  (set-condvar-waiters! cvar (cons waiter waiters)))

(define (remove-condvar-waiter! cvar waiter)
  (define waiters (condvar-waiters cvar))
  (set-condvar-waiters! cvar (remq waiter waiters)))

(define (signal-next-waiter! cvar)
  (define waiters (condvar-waiters cvar))
  (and (not (null? waiters))
       (let ([waiter (random-ref waiters)])
         (remove-condvar-waiter! cvar waiter)
         (semaphore-post waiter)
         waiter)))
