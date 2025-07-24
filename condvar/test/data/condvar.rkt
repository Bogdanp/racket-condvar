#lang racket/base

(require data/condvar
         rackunit)

(define condvar-tests
  (test-suite
   "condvar"

   (test-case "signal"
     (define c (make-condvar))
     (define n 0)
     (define mu (make-semaphore 1))
     (define (make-incrementer)
       (thread
        (lambda ()
          (semaphore-wait mu)
          (sync (condvar-wait-evt c mu))
          (set! n (add1 n))
          (semaphore-post mu))))
     (define thd1 (make-incrementer))
     (define thd2 (make-incrementer))
     (sync (system-idle-evt))
     (check-equal? n 0)
     (condvar-signal c)
     (sync (system-idle-evt))
     (check-equal? n 1)
     (condvar-signal c)
     (sync (system-idle-evt))
     (check-equal? n 2)
     (check-true (thread-dead? thd1))
     (check-true (thread-dead? thd2)))

   (test-case "broadcast"
     (define c (make-condvar))
     (define n 0)
     (define mu (make-semaphore 1))
     (define (make-incrementer)
       (thread
        (lambda ()
          (semaphore-wait mu)
          (sync (condvar-wait-evt c mu))
          (set! n (add1 n))
          (semaphore-post mu))))
     (define thd1 (make-incrementer))
     (define thd2 (make-incrementer))
     (sync (system-idle-evt))
     (check-equal? n 0)
     (condvar-broadcast c)
     (sync (system-idle-evt))
     (check-equal? n 2)
     (check-true (thread-dead? thd1))
     (check-true (thread-dead? thd2)))

   (test-case "nack"
     (define c (make-condvar))
     (define ok? #t)
     (define mu (make-semaphore 1))
     (define thd
       (thread
        (lambda ()
          (with-handlers ([exn:break? void])
            (semaphore-wait mu)
            (sync (condvar-wait-evt c mu))
            (set! ok? #f)
            (semaphore-post mu)))))
     (sync (system-idle-evt))
     (check-true ok?)
     (break-thread thd)
     (sync (system-idle-evt))
     (check-true ok?))

   (test-case "fairness"
     (define cvar (make-condvar))
     (define mu (make-semaphore 1))
     (define (make-counter)
       (define n 0)
       (thread
        (lambda ()
          (let loop ()
            (semaphore-wait mu)
            (condvar-wait cvar mu)
            (semaphore-post mu)
            (set! n (add1 n))
            (loop))))
       (λ () n))
     (define get-c1 (make-counter))
     (define get-c2 (make-counter))
     (for ([_ (in-range 500)])
       (sync (system-idle-evt))
       (condvar-signal cvar))
     (semaphore-wait mu)
     (check-true ((get-c1) . > . 200))
     (check-true ((get-c2) . > . 200)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests condvar-tests))
