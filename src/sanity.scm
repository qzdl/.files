(define-module (sanity)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1))

(format #t "\n\nchecking emacs\n\n")
(use-modules (emacs))

(format #t "\n\nchecking user header\n\n")
(use-modules (header))
