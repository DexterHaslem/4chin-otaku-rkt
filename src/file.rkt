#lang racket
; file management helpers
(require net/url)
(require "4chan-api.rkt")
(require racket/path)

(provide download-post-file
         post->save-location)

;> (path->string (path-only (build-path "part1" "part2" "test.txt")))
; change this to change where things get saved to - must be writable (particularly problematic on windows users)
(define root-download-folder (current-directory))

(define (create-paths-for-post post)
  ; make sure root/board/ folder exists
  empty)

(define (download-url url path)
  (define dlport (get-pure-port (string->url url)))
  (define outfile (open-output-file path #:mode 'binary))
  (copy-port dlport outfile)
  (close-output-port outfile)
  (close-input-port dlport))

; when given a post object,
; it digs out the post url,
; then returns current path + "\board\postfile.ext"
(define (post->save-location post)
  (build-path root-download-folder (post->boardname post) (post->local-file post)))


(define (download-post-file post)
  (define target-path (post->save-location post))
  (define target-dir (path-only target-path))
  ;first make sure our directory exists
  (unless (directory-exists? target-dir)
    (make-directory target-dir))
  (download-url (post->file-url post) target-path))

;(define (tdl)
;  (download-url "http://puu.sh/9ugod/f362c6ab47.png" "C:\\Users\\Dexter\\Pictures\\test.png"))
