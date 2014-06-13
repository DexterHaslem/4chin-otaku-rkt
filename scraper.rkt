#lang racket
(require net/url)
(require json)

(define (get-url-json url)
  (string->jsexpr (port->string (get-pure-port (string->url url)))))

(define (fetch-catalog board) 
  (get-url-json (format "http://a.4cdn.org/~a/catalog.json" board)))

; https://github.com/4chan/4chan-API
; number, subject, comment, file ext, file timestamp (renamed name), replies count, imgs# count
(struct thread (no sub com ext tim replies# imgs#) #:transparent)

(define (thread-json->thread hash)
  (thread 
   (if (hash-has-key? hash 'no) (hash-ref hash 'no) 0)
   (if (hash-has-key? hash 'sub) (hash-ref hash 'sub) "")
   (if (hash-has-key? hash 'com) (hash-ref hash 'com) "")
   (if (hash-has-key? hash 'ext) (hash-ref hash 'ext) "")
   (if (hash-has-key? hash 'tim) (hash-ref hash 'tim) "")
   (if (hash-has-key? hash 'replies) (hash-ref hash 'replies) "")
   (if (hash-has-key? hash 'images) (hash-ref hash 'images) "")))

(define (walk-catalog json)
  ; top level json isnt actually a hash, its a list of hashes by page#
  (when (not (empty? json))
    (let* ((cur-page (first json))
           (pagenum (hash-ref cur-page 'page))
           (threads (hash-ref cur-page 'threads)))
      (define parsed-threads (map parse-page-thread threads))
      (map 
       (lambda (t) 
         (printf "thread subject='~a' comment='~a' #imgs=~a #replies=~a ~n"
                 (thread-sub t) (thread-com t) (thread-imgs# t) (thread-replies# t))) parsed-threads)
      (walk-catalog (rest json)))))

(define (parse-page-thread thread-hash)
  (if (hash? thread-hash)
      (thread-json->thread thread-hash)
      'bad-thread))

;;http://a.4cdn.org/gif/catalog.json
;(define anime (fetch-catalog "a"))