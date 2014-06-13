#lang racket/gui
(define frame (new frame% 
                   [label "debug"]
                   [width 300]
                   [height 400]))

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

(define choice (new choice%
                    [label "Board"]
                    [parent frame]
                    [choices (list "a" "b" "c" "d" "e" "f" "g" "gif" "h" "hr")]))

(define fetch-button (new button% 
                          [parent frame] 
                          [label "fetch"]))

(define msg-text (new message% 
                      [parent frame]
                      [label "msg-text"]))
(send frame show #t)