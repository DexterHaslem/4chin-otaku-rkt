; 4chan-api.rkt 
; simple parsing of the 4chan JSON api, see https://github.com/4chan/4chan-API

#lang racket

(require net/url)
(require json)

; all the API endpoints are here in one spot for easy updating
(define api-boards-url "http://a.4cdn.org/boards.json")
(define api-catalog-url "http://a.4cdn.org/~a/catalog.json") ; board
(define api-threads-url "http://a.4cdn.org/~a/threads.json") ; board
(define api-thread-url "http://a.4cdn.org/~a/thread/~a.json") ; board thread#
(define api-image-url "http://i.4cdn.org/~a/~a~a") ;board tim ext (dont include the dot in url, ext has it)

(provide get-boards 
         get-board-threads
         get-thread-posts
         get-thread-posts-with-files
         post->string
         board->string
         post->file-url
         get-all-board-file-urls)

(define (get-url-json url)
  (string->jsexpr (port->string (get-pure-port (string->url url)))))

(struct board (name title))

(define (board->string b)
  (format "~a / ~a" [board-name b] [board-title b]))

(define (get-boards-json)
  (get-url-json api-boards-url))

(define (get-boards)
  (define (get-boards-list board-json-hash)
    (cond
      [(empty? board-json-hash) empty]
      [else 
       (define cur-chunk (first board-json-hash))
       (cons (board [hash-ref cur-chunk 'board] [hash-ref cur-chunk 'title]) 
             (get-boards-list (rest board-json-hash)))]))
  (get-boards-list (hash-ref [get-boards-json] 'boards)))

(define (get-board-threadlist-json board)
  (get-url-json (format api-threads-url [board-name board])))

(struct board-thread (no lastmodified board))

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

; the post struct only contains things we care about - details to grab the file
; there is a reference to thread that owns it (which contains board as well) so we can
; reverse-lookup everything to poop out a final file url
(struct post (no ext tim fsize thread))

(define (post->string p)
  (format "post no=~a file=~a~a" (post-no p) (post-tim p) (post-ext p)))

(define (get-thread-json board thread-no#)
  (get-url-json (format api-thread-url [board-name board] thread-no#)))

(define (post-hash->post posthash thread)
  ; if a post doesnt have a file it will not have ext and tim and filesize so we need to be careful
  (post
   [hash-ref posthash 'no]
   (if (hash-has-key? posthash 'ext) [hash-ref posthash 'ext] "")
   (if (hash-has-key? posthash 'tim) [hash-ref posthash 'tim] "")
   (if (hash-has-key? posthash 'fsize) [hash-ref posthash 'fsize] 0)
   thread))

; broke this down for debugging, predicate was always returning false..
(define (post-has-files? p)
   (> (post-fsize p) 0)); simply use file size as some garentee the other two will be present

(define (get-thread-posts thread)
  (define thread-json [get-thread-json (board-thread-board thread) (board-thread-no thread)])
  (define posts-json (hash-ref thread-json 'posts))
  (define (iterate-posts lst)
    (cond
      [(empty? lst) empty]
      [else
       (cons [post-hash->post (first lst) thread] [iterate-posts (rest lst)])]))
   (iterate-posts posts-json))
 
(define (get-thread-posts-with-files t)
  (define all-posts (get-thread-posts t))
  (filter post-has-files? all-posts))

(define (post->file-url post)
  ; we need to dig out the board from the post so we can stick it in the url
  (define thread [post-thread post])
  (define board [board-thread-board thread])
  (define bname [board-name board])
  (define url [format api-image-url bname [post-tim post] [post-ext post]])
  url)
  
(define (get-all-board-file-urls board)
  (define start-seconds (current-seconds))
  (define board-threads (get-board-threads board))
  (define thread-posts-list (flatten (map get-thread-posts-with-files board-threads)))
  (define end-seconds (current-seconds))
  (printf (format "got ~a posts in ~a seconds~n" (length thread-posts-list) (- end-seconds start-seconds)))
  (map post->file-url thread-posts-list))

(define (test-read-all-post-files)
  (get-all-board-file-urls (first (get-boards))))
