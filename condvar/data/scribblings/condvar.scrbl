#lang scribble/manual

@(require (for-label data/condvar
                     racket/base
                     racket/contract))

@title{Condition Variables}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[data/condvar]

@(define reference-doc '(lib "scribblings/reference/reference.scrbl"))
@(define semaphores (tech #:doc reference-doc "semaphores"))
@(define event (tech #:doc reference-doc "synchronizable event"))

This module provides an implementation of @deftech{Condition Variables}
built on top of Racket's @|semaphores|.

@deftogether[(
  @defproc[(condvar? [v any/c]) boolean?]
  @defproc[(make-condvar) condvar?]
)]{
  The @racket[make-condvar] procedure returns a new @tech{condition
  variable}.
}

@defproc[(condvar-signal [cvar condvar?]) void?]{
  Wakes up one of @racket[cvar]'s waiters.
}

@defproc[(condvar-broadcast [cvar condvar?]) void?]{
  Wakes up all of @racket[cvar]'s waiters.
}

@defproc[(condvar-wait-evt [cvar condvar?]
                           [mutex semaphore?]) evt?]{
  Returns a @event that may become ready for synchronization when the
  condition variable is signaled.

  The @racket[mutex] argument must be a semaphore whose internal
  counter is zero at the time @racket[condvar-wait-evt] is called.
  This procedure increments the @racket[mutex] internally after adding
  the waiter to the waitlist, then decrements it once the condition is
  signaled.
}

@defproc[(condvar-wait [cvar condvar?]
                       [mutex semaphore?]) void?]{
  Equivalent to @racket[(sync/enable-break (condvar-wait-evt cvar mutex))].
}
