#lang racket/gui

; debug ui, will likely change to something better once it actuall works 

(require "4chan-api.rkt")
(require "file.rkt")

; vars
; this is from api

(define available-boards (get-boards))

(define selected-board (first available-boards))
(define selected-ext (first api-file-exts))
(define posts empty)
(define filtered-posts empty)
(define filtered-count 0)

; ui

(define frame (new frame% 
                   [label "4chin-otaku"]
                   [width 400]
                   [height 50]))


(define main-panel (new vertical-panel% 
                        [parent frame]
                        [alignment '(center top)]))

(define status-msg (new message%
                        [parent frame]
                        [min-width 400]
                        [label ""]))

(define choice-panel (new horizontal-panel% 
                          [parent main-panel]
                          [alignment '(left top)]
                          [horiz-margin 5]
                          [min-height 35]))

(define board-choice (new choice%
                          [label "Board "]
                          [parent choice-panel]
                          [min-height 30]
                          [choices (map [lambda (b) (board->string b)] available-boards)]
                          [callback (lambda (c e)
                                      (set! selected-board [list-ref available-boards (send board-choice get-selection)]))]))

(define ext-choice (new choice%
                        [label "ext "]
                        [parent choice-panel]
                        [choices api-file-exts]
                        [min-height 30]
                        [callback (lambda (c e)
                                    (set! selected-ext [list-ref api-file-exts (send ext-choice get-selection)])
                                    (update-filtered-posts))]))

(define (on-get-posts b e)
  (send status-msg set-label (format "getting all posts for board \"~a\" ...~n" (board->string selected-board)))
  (define cur-post-count 0)
  (get-all-board-posts-mt selected-board
                          
                          (lambda (thread-progress)
                            (set! cur-post-count (+ cur-post-count (length thread-progress)))
                            (queue-callback (lambda () (send status-msg set-label (format "received ~a posts.." cur-post-count)))))
                          
                          ; note that this needs to be queue-callback'd as well, a race condition can occur 
                          ; and the 'received posts..' update could come after
                          (lambda (newposts)
                            (set! posts newposts)
                            (queue-callback (lambda () (send status-msg set-label (format "got ~a total posts with files~n" (length posts)))))
                            (update-filtered-posts))))

(define (update-filtered-posts)
  ; filtering is a two step process
  ; 1 -> file ext
  ; 2 -> checking if file exists (save only files that aren't already on disk)
  (define ext-filtered
    (filter 
     (lambda (p) 
       (define p-ext (post-ext p))
       (define sxt selected-ext)
       (cond
         [(string-ci=? sxt "*.*") #t] ; they want all files so no actual filter
         [(string-ci=? p-ext sxt) #t]
         [else #f])) posts))
  (define new-posts
    (filter
     (lambda (p)
       (not (file-exists? (post->save-location p))))
     ext-filtered))
  (set! filtered-posts new-posts)
  (set! filtered-count (length filtered-posts))
  (send download-button set-label (format "download (~a)" filtered-count)))

(define (on-download-posts b e)
  (thread 
   (lambda ()
     (map (lambda (p)
             [unless (file-exists? (post->save-location p))
               (sleep 0.01)
               (send status-msg set-label (format "downloading file ~a...~n" (post->local-file p)))
               (download-post-file p)
               
               (queue-callback [lambda ()
                                 (sub1 filtered-count)
                                 (send download-button set-label (format "download (~a)" filtered-count))])])
           filtered-posts)
      (queue-callback (lambda () 
                        [send status-msg set-label (format "done downloading ~a total new post files ~n" [length filtered-posts])]))
      ;update filtered here so if they click again it doesnt blow up
      (update-filtered-posts))))

(define get-posts-button (new button% 
                              [callback on-get-posts]
                              [parent choice-panel] 
                              [label "get posts"]
                              [vert-margin 5]
                              [min-height 40]))

(define download-button (new button% 
                             [callback on-download-posts]
                             [parent choice-panel] 
                             [label "download (0)"]                   
                             [vert-margin 5]
                             [min-height 40]))



(send frame show #t)
