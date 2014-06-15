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
(struct catalog-threadinfo (no sub com ext tim replies# imgs# owner-board) #:transparent)

; todo - remove stuff we dont care about (and possibly catalogs in general)
(define (catalog-thread-json->catalog-threadinfo hash board)
  (catalog-threadinfo 
   (if (hash-has-key? hash 'no) (hash-ref hash 'no) 0)
   (if (hash-has-key? hash 'sub) (hash-ref hash 'sub) "")
   (if (hash-has-key? hash 'com) (hash-ref hash 'com) "")
   (if (hash-has-key? hash 'ext) (hash-ref hash 'ext) "")
   (if (hash-has-key? hash 'tim) (hash-ref hash 'tim) "")
   (if (hash-has-key? hash 'replies) (hash-ref hash 'replies) "")
   (if (hash-has-key? hash 'images) (hash-ref hash 'images) ""))
  board)

(define (parse-page-threadinfo thread-hash owner-board)
  (if (hash? thread-hash)
      (catalog-thread-json->catalog-threadinfo thread-hash owner-board)
      'bad-thread))

(define (get-catalog-json board) 
  (get-url-json (format "http://a.4cdn.org/~a/catalog.json" [board-name board])))

; returns a list of lists, page - threads
(define (get-board-catalog board)
  (define catalog-json (get-catalog-json board))
  ; a list of pages, and each page has a list of threads (all hashes)
  (define (get-page-threads page-json)
    (define threads [hash-ref page-json 'threads])
    (map (lambda (t) 
           (parse-page-threadinfo threads board))))
  
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

(struct board-thread (no lastmodified board) #:transparent)

(define (board-thread->string b)
   (format "thread no#=~a last_modified=~a" [board-thread-no b] [board-thread-lastmodified b]))

(define (get-board-threads board)
  (define board-thread-json (get-board-threadlist-json board))
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
            (cons [board-thread (hash-ref cur-thread 'no) (hash-ref cur-thread 'last_modified) board]
                  [walk-threads (rest thread-list)])]))
       (cons (walk-threads threads) (parse-pages (rest lst)))]))
  (flatten (parse-pages board-thread-json)))

;(define (test-get-board-threads)
;  (get-board-threads (first (get-boards))))

#| thread 'post' code - a thread is just a list of posts
   each post section looks something like this:
{
            "no": 6326151,
            "now": "06/14/14(Sat)17:22:51",
            "name": "Anonymous",
            "filename": "tumblr_n73jm0d4nH1tw0p0po1_500",
            "ext": ".gif",
            "w": 500,
            "h": 250,
            "tn_w": 125,
            "tn_h": 62,
            "tim": 1402780971833,
            "time": 1402780971,
            "md5": "pr8H7r7R8im6jenRuu+wSw==",
            "fsize": 1098257,
            "resto": 6326147
        },
thread json is a posts element with list of .. posts.
|#

; the post struct only contains things we care about - details to grab the file
; there is a reference to thread that owns it (which contains board as well) so we can
; reverse-lookup everything to poop out a final file url
(struct post (no ext tim fsize owner-thread))

(define (get-thread-json board thread-no#)
  (get-url-json (format "http://a.4cdn.org/~a/thread/~a.json" [board-name board] thread-no#)))

(define (post-hash->post posthash thread)
  (post
   [hash-ref posthash 'no]
   [hash-ref posthash 'tim]
   [hash-ref posthash 'fsize]
   thread))

(define (get-thread-posts thread)
  (define thread-json [get-thread-json (board-thread-board thread) (board-thread-no thread)])
  (define posts-json (hash-ref thread-json 'posts))
  (define (iterate-posts lst)
    (cond
      [(empty? lst) empty]
      [else
       (cons [post-hash->post (first lst) thread] [iterate-posts (rest lst)])]))
   (iterate-posts posts-json))
    