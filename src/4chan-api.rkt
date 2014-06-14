#lang racket

(require net/url)
(require json)

(provide get-boards board board->string)

(define (get-url-json url)
  (string->jsexpr (port->string (get-pure-port (string->url url)))))

(define (fetch-catalog-json board) 
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

; top level category json isnt actually a hash, its a list of hashes by page#
(define (get-threads-list board-catalog-json)
  (cond 
    [(empty? board-catalog-json) empty]
    [else
     (define cur-chunk [first board-catalog-json])
     (define threads [hash-ref cur-chunk 'threads])
     (cons (map parse-page-thread threads) (get-threads-list (rest board-catalog-json)))]))

(define (parse-page-thread thread-hash)
  (if (hash? thread-hash)
      (thread-json->thread thread-hash)
      'bad-thread))

;;http://a.4cdn.org/boards.json
(struct board (name title) #:transparent)

(define (board->string b)
  (format "~a / ~a" (board-name b) (board-title b)))

(define (fetch-boards-json)
  (get-url-json "http://a.4cdn.org/boards.json"))

(define (get-boards)
  (get-boards-list (hash-ref [fetch-boards-json] 'boards)))

(define (get-boards-list board-json-hash)
  (cond
    [(empty? board-json-hash) empty]
    [else 
     (define cur-chunk (first board-json-hash))
     (cons (board [hash-ref cur-chunk 'board] [hash-ref cur-chunk 'title]) 
           (get-boards-list (rest board-json-hash)))]))
;lst)]))

;(map 
;     [lambda (t) 
;      
;     boards-json))

; (hash-ref (first (hash-ref (fetch-boards-json) 'boards)) 'title)
;"3DCG"

;;http://a.4cdn.org/gif/catalog.json
;(define anime (fetch-catalog "a"))

