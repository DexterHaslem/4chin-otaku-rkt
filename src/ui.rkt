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
                                (printf "selected= ~a~n" (send choice get-selection)))]))

(define (debug-print-catalog lst)
  (send text insert (format "got ~a pages:~n" [length lst]))
  (map (lambda (p)
         (send text insert (format "~a threads~n"  (length p)))
         ) lst))
 
(define (on-fetch-threads button event)
  (send text insert (format "getting threads for board \"~a\" ...~n" (board->string selected-board)))
  (define selected-board-catalog (get-board-catalog-list selected-board))
  (debug-print-catalog selected-board-catalog))

(define fetch-button (new button% 
                          [callback on-fetch-threads]
                          [parent choice-panel] 
                          [label "Get threads"]))

(send frame show #t)
