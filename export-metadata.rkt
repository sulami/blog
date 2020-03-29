#!/usr/bin/env racket
#lang racket

(require threading)

(define (complement f)
  (λ args
    (not (apply f args))))

(define is-file?
  (complement directory-exists?))

(define (org->md-metadata lines)
  (map (λ (l)
         (define-values (k v) (apply values (string-split l ":")))
         (string-append (~> k
                            (string-downcase)
                            (substring 2))
                        ":" v))
       lines))

(define (export-metadata file-name)
  (let ((header (~> file-name
                    (file->string)
                    (string-split "\n")
                    (takef (λ (l) (string-prefix? l "#+")))
                    (org->md-metadata)
                    (string-join "\n")))
        (metadata-file-name (string-append file-name ".metadata")))
    (printf "~a => ~a\n" file-name metadata-file-name)
    (display-to-file header
                     metadata-file-name
                     #:exists 'truncate)))

(~>> "content"
     (in-directory)
     (sequence->list)
     (filter is-file?)
     (map path->string)
     (filter (λ (fp) (string-suffix? fp ".org")))
     (map export-metadata))
