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

; ui

(define frame (new frame% 
                   [label "debug ui"]
                   [width 400]
                   [height 350]))

; menu
(define (on-menu-quit o c)
  (send frame on-exit))
(define menu (new menu-bar% [parent frame]))
(define menu-file (new menu% 
                       [label "&File"]
                       [parent menu]))
(new menu-item% 
     [parent menu-file]
     [label "Quit"]
     [callback on-menu-quit])

(define main-panel (new vertical-panel% 
                          [parent frame]
                          [alignment '(center top)]))

(define debug-editor (new editor-canvas% 
                      [parent main-panel]
                      [min-height 265]
                      [label "f"]))

(define text (new text%));text%))
(send debug-editor set-editor text)

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
  (send text insert (format "getting all posts for board \"~a\" ...~n" (board->string selected-board)))
  (set! posts (get-all-board-posts selected-board))
  (send text insert (format "got ~a total posts with files~n" (length posts)))
  (update-filtered-posts))

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
  (send download-button set-label (format "download matching (~a)" (length filtered-posts))))

(define (on-download-posts b e)
  (map (lambda (p)
         (send text insert (format "downloading file ~a...~n" (post->local-file p)))
         ; we need to enforce a delay here to not get throttled
         (sleep 0.025) ; 25ms
         ; check again here in case user pressed button twice
         (unless (file-exists? (post->save-location p))
           (download-post-file p)))
         filtered-posts)
   (send text insert (format "done downloading ~a total new post files ~n" [length filtered-posts])))

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
