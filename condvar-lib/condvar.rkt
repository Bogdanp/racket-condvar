#lang racket/base

(require racket/treelist)

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
   #;waiters-box (box (treelist))))

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
  (let loop ([acquired? #f]
             [waiters (unbox waiters-box)])
    (define waiters*
      (proc waiters))
    (cond
      [(box-cas! waiters-box waiters waiters*) waiters*]
      [(eq? waiters (unbox waiters-box)) (loop acquired? waiters)]
      [acquired? (loop acquired? (unbox waiters-box))]
      [else (call-with-semaphore mu
              (lambda ()
                (loop #t (unbox waiters-box))))])))

(define (add-condvar-waiter! cvar waiter)
  (update-waiters! cvar (λ (waiters) (treelist-add waiters waiter))))

;; XXX: The proc argument to update-waiters! may be called multiple
;; times, so make sure to reset the result box if waiters is ever empty.
(define (pop-condvar-waiter! cvar)
  (define waiter-box (box #f))
  (update-waiters!
   cvar (λ (waiters)
          (cond
            [(treelist-empty? waiters)
             (set-box! waiter-box #f)
             waiters]
            [else
             (define waiter (treelist-first waiters))
             (set-box! waiter-box waiter)
             (treelist-rest waiters)])))
  (unbox waiter-box))

(define (remove-condvar-waiter! cvar waiter)
  (update-waiters! cvar (λ (waiters) (treelist-remove waiters waiter))))

;; Guaranteed not to signal spuriously since waiter is removed from the
;; waitlist atomically by pop-condvar-waiter!.
(define (signal-next-waiter! cvar)
  (define waiter (pop-condvar-waiter! cvar))
  (when waiter (semaphore-post waiter))
  waiter)

(define (treelist-remove tl v)
  (for/treelist ([w (in-treelist tl)]
                 #:unless (eq? w v))
    w))
