; 4chan-api.rkt 
; simple parsing of the 4chan JSON api, see https://github.com/4chan/4chan-API

#lang racket

(require net/url)
(require json)

(provide get-boards board->string
         get-board-catalog
         get-board-catalog-threadinfos
         get-board-threads
         board-thread->string)
;(provide [struct-out board])

(define (get-url-json url)
  (string->jsexpr (port->string (get-pure-port (string->url url)))))

;;http://a.4cdn.org/boards.json
(struct board (name title) #:transparent)

(define (board->string b)
  (format "~a / ~a" [board-name b] [board-title b]))

(define (get-boards-json)
  (get-url-json "http://a.4cdn.org/boards.json"))

(define (get-boards)
  (define (get-boards-list board-json-hash)
    (cond
      [(empty? board-json-hash) empty]
      [else 
       (define cur-chunk (first board-json-hash))
       (cons (board [hash-ref cur-chunk 'board] [hash-ref cur-chunk 'title]) 
             (get-boards-list (rest board-json-hash)))]))
  (get-boards-list (hash-ref [get-boards-json] 'boards)))

; number, subject, comment, file ext, file timestamp (renamed name), replies count, imgs# count
(struct catalog-threadinfo (no sub com ext tim replies# imgs#) #:transparent)

(define (catalog-thread-json->catalog-threadinfo hash)
  (catalog-threadinfo 
   (if (hash-has-key? hash 'no) (hash-ref hash 'no) 0)
   (if (hash-has-key? hash 'sub) (hash-ref hash 'sub) "")
   (if (hash-has-key? hash 'com) (hash-ref hash 'com) "")
   (if (hash-has-key? hash 'ext) (hash-ref hash 'ext) "")
   (if (hash-has-key? hash 'tim) (hash-ref hash 'tim) "")
   (if (hash-has-key? hash 'replies) (hash-ref hash 'replies) "")
   (if (hash-has-key? hash 'images) (hash-ref hash 'images) "")))

(define (parse-page-threadinfo thread-hash)
  (if (hash? thread-hash)
      (catalog-thread-json->catalog-threadinfo thread-hash)
      'bad-thread))

(define (get-catalog-json board) 
  (get-url-json (format "http://a.4cdn.org/~a/catalog.json" [board-name board])))

; returns a list of lists, page - threads
(define (get-board-catalog board)
  (define catalog-json (get-catalog-json board))
  ; a list of pages, and each page has a list of threads (all hashes)
  (define (get-page-threads page-json)
    (define threads [hash-ref page-json 'threads])
    (map parse-page-threadinfo threads))
  
  (define (get-all-page-threads lst)
    (cond
      [(empty? lst) empty]
      [else
       (cons (get-page-threads [first lst]) (get-all-page-threads (rest lst)))]))
  (get-all-page-threads catalog-json))

; we likely have a ui / consumer that doesnt care about what page a thread is on
(define (get-board-catalog-threadinfos catalog)
  (flatten catalog))

(define (get-board-threadlist-json board)
  (get-url-json (format "http://a.4cdn.org/~a/threads.json" [board-name board])))

(struct board-thread (no lastmodified) #:transparent)

(define (board-thread->string b)
   (format "board no#=~a last_modified=~a" [board-thread-no b] [board-thread-lastmodified b]))

(define (get-board-threads board)
  (define board-thread-json (get-board-threadlist-json board))
  (define ret empty)
  ; again, list of pages with list of threads
  (define (parse-pages lst)
    (cond
      [(empty? lst) empty]
      [else
       (define threads [hash-ref (first lst) 'threads])
       (define (walk-threads thread-list)
         (cond
           [(empty? thread-list) empty]
           [else 
            (define cur-thread [first thread-list])
            (cons [board-thread (hash-ref cur-thread 'no) (hash-ref cur-thread 'last_modified)]
                  [walk-threads (rest thread-list)])]))
       (cons (walk-threads threads) (parse-pages (rest lst)))]))
  (flatten (parse-pages board-thread-json)))

(define (test-get-board-threads)
  (get-board-threads (first (get-boards))))
    