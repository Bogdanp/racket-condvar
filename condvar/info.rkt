#lang info

(define license 'BSD-3-Clause)
(define collection 'multi)
(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))
(define deps
  '("base"
    "condvar-lib"
    "rackunit-lib"))
(define implies
  '("condvar-lib"))
