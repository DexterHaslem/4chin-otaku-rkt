#lang racket/gui
(require "4chan-api.rkt")

(define frame (new frame% 
                   [label "debug ui"]
                   [width 500]
                   [height 400]))

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

(define available-boards (get-boards))
(define selected-board (first available-boards))

(define main-panel (new vertical-panel% 
                          [parent frame]
                          [alignment '(center top)]))

(define debug-editor (new editor-canvas% 
                      [parent main-panel]
                      [min-height 300]
                      [label "f"]))
(define text (new text%))
(send debug-editor set-editor text)

(define choice-panel (new horizontal-panel% 
                          [parent main-panel]
                          [alignment '(left top)]
                          [horiz-margin 5]))

(define choice (new choice%
                    [label "Board "]
                    [parent choice-panel]
                    ;[vert-margin 10]
                    [choices (map [lambda (b) (board->string b)] available-boards)]
                    [callback (lambda (c e)
                                ;(define selected-board (first (string-split (send choice get-selection) ":")))
                                (set! selected-board (list-ref available-boards (send choice get-selection)))
                                ;(printf "selected= ~a~n" (send choice get-selection))
                                )]))
 
(define (on-get-posts button event)
  (send text insert (format "getting threads for board \"~a\" ...~n" (board->string selected-board)))
  ; removed catalog crap - way more verbose thread info than needed
  ; using the get-board-threads api is much faster since it just returns thread no and last modified
  (define board-threads (get-board-threads selected-board))
  (send text insert (format "got ~a threads~n" [length board-threads]))
  ;(send text insert (format "got ~a total polsts~n" 
  ;                          [foldl + 0 ))
  (map (lambda (t)
         (define thread-posts (get-thread-posts-with-files t))
         (send text insert (format "got ~a posts in thread~n" (length thread-posts))))
       board-threads))

(define fetch-button (new button% 
                          [callback on-get-posts]
                          [parent choice-panel] 
                          [label "Get all posts"]))

(send frame show #t)
