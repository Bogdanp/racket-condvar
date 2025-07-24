#lang racket/base

(require racket/list)

(provide
 make-condvar
 condvar?
 condvar-signal
 condvar-broadcast
 condvar-wait-evt
 condvar-wait)

(struct condvar (mu waiters-box))

(define (make-condvar)
  (condvar
   #;mu (make-semaphore 1)
   #;waiters-box (box null)))

(define (condvar-signal cvar)
  (void (signal-next-waiter! cvar)))

(define (condvar-broadcast cvar)
  (when (signal-next-waiter! cvar)
    (void (condvar-broadcast cvar))))

(define (condvar-wait-evt cvar mutex)
  (when (semaphore-try-wait? mutex)
    (error 'condvar-wait-evt "mutex open"))
  (nack-guard-evt
   (lambda (nack)
     (define waiter (make-semaphore))
     (add-condvar-waiter! cvar waiter)
     (thread
      (lambda ()
        (sync nack)
        (remove-condvar-waiter! cvar waiter)))
     (semaphore-post mutex)
     (replace-evt waiter (λ (_) mutex)))))

(define (condvar-wait cvar mutex)
  (sync/enable-break (condvar-wait-evt cvar mutex)))

(define (update-waiters! cvar proc)
  (define mu (condvar-mu cvar))
  (define waiters-box
    (condvar-waiters-box cvar))
  (parameterize-break #f
    (let loop ([locked? #f]
               [waiters (unbox waiters-box)])
      (cond
        [(box-cas! waiters-box waiters (proc waiters))
         (when locked?
           (semaphore-post mu))]
        [(eq? waiters (unbox waiters-box))
         (loop #f waiters)]
        [else
         (semaphore-wait mu)
         (loop #t (unbox waiters-box))]))))

(define (add-condvar-waiter! cvar waiter)
  (update-waiters! cvar (λ (waiters) (cons waiter waiters))))

;; XXX: The proc argument to update-waiters! may be called multiple
;; times, so make sure to reset the result box if waiters is ever null.
(define (pop-condvar-waiter! cvar)
  (define waiter-box (box #f))
  (update-waiters!
   cvar (λ (waiters)
          (cond
            [(null? waiters)
             (set-box! waiter-box #f)
             null]
            [else
             (define waiter (last waiters))
             (set-box! waiter-box waiter)
             (remq waiter waiters)])))
  (unbox waiter-box))

(define (remove-condvar-waiter! cvar waiter)
  (update-waiters! cvar (λ (waiters) (remq waiter waiters))))

;; Guaranteed not to signal spuriously since waiter is removed from the
;; waitlist atomically by pop-condvar-waiter!.
(define (signal-next-waiter! cvar)
  (define waiter (pop-condvar-waiter! cvar))
  (when waiter (semaphore-post waiter))
  waiter)
