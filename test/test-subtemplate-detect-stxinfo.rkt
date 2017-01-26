#lang racket
(module m-ok racket
  (require subtemplate
           stxparse-info/parse
           stxparse-info/case
           rackunit
           syntax/macro-testing)
  (check-not-exn
   (λ ()
     (convert-compile-time-error
      (subtemplate ok)))))

(module m-no-parse racket
  (require subtemplate
           stxparse-info/case
           rackunit
           syntax/macro-testing)
  (check-exn #rx"subtemplate: syntax-parse seems undefined,"
   (λ ()
     (convert-compile-time-error
      (subtemplate ok)))))

(module m-wrong-parse racket
  (require subtemplate
           syntax/parse
           stxparse-info/case
           rackunit
           syntax/macro-testing)
  (check-exn
   #rx"subtemplate: syntax-parse resolves to the official syntax-parse,"
   (λ ()
     (convert-compile-time-error
      (subtemplate ok)))))

(module m-wrong-case racket
  (require subtemplate
           stxparse-info/parse
           rackunit
           syntax/macro-testing)
  (check-exn #rx"subtemplate: syntax-case resolves to the official syntax-case,"
   (λ ()
     (convert-compile-time-error
      (subtemplate ok)))))

(module m-no-parse-wrong-case racket
  (require subtemplate
           rackunit
           syntax/macro-testing)
  (check-exn #rx"subtemplate: syntax-parse seems undefined,"
   (λ ()
     (convert-compile-time-error
      (subtemplate ok)))))

(module m-wrong-parse-wrong-case racket
  (require subtemplate
           syntax/parse
           rackunit
           syntax/macro-testing)
  (check-exn
   #rx"subtemplate: syntax-parse resolves to the official syntax-parse,"
   (λ ()
     (convert-compile-time-error
      (subtemplate ok)))))


(require 'm-ok)
(require 'm-no-parse)
(require 'm-wrong-parse)
(require 'm-wrong-case)
(require 'm-no-parse-wrong-case)
(require 'm-wrong-parse-wrong-case)
