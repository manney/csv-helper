#lang racket

;; csv-helper.rkt
;; A few methods to help parse CSV files roughly based off of RFC4180
;; (C) 2015 By Aaron Sebold (Manny)
;; Released under the GPL v3

(define (pull-csv filename)
  ;; Opens a CSV file and returns a list of each line
  (call-with-input-file filename
    (lambda (in)
      (let loop ((x (read-line in)))
        (cond
          ((eof-object? x) '())
          (else
           (cons x (loop (read-line in)))))))))

(define (separate-line line separator)
  ;; Separates a 'line' of CSV in to a list of cells
  ;; where 'line' is a string of a row of cells
  ;; and 'separator' is the character that divides the cells
  (let ((in (open-input-string line)))
    (let string-loop ((c (read-char in))
                      (s "")
                      (in-quote #f)
                      (double-quote #f))
      (cond
        ; If we hit the end of the string, return the list of cells
        ((eof-object? c)
         ; Check to see if we are missing a trailing #\"
         ; add it if we are
         (if (or in-quote double-quote)
             `(,(string-append s "#\""))
             `(,s)))
        ; If we hit a separator (while not in a quote)
        ; add the new string and recur
        ((and (char=? c separator) (not in-quote))
         (cons s (string-loop (read-char in) "" in-quote double-quote)))
        ; This is where all of the work is done:
        ; 1. If we hit a quote, change 'in-quote' to #t or #f depending
        ;    it's value: add it to the new string, recur
        ; 2. If it's not 1, then: add it to the new string, recur
        ; 3. We know it's not going to be a separator because of
        ;    the first check in the (cond)
        ((char=? c #\")
         (cond
           ; Check for a double quote
           ; If it's a double quote then: recur, add it to the new string
           ((and (not double-quote)
                 (not (eof-object? (peek-char in)))
                 (char=? (peek-char in) #\"))
            (set! double-quote #t))
           ; We're already in a double quote:
           ; reset the flag, recur, add it to the new string
           (double-quote
            (set! double-quote #f))
           ; Must be a lone quote, so just reverse 'in-quote' and recur
           ((and (not in-quote) (not double-quote))
            (set! in-quote #t))
           (else
             (set! in-quote #f)))
         (string-loop (read-char in) (string-append s (string c))
                      in-quote double-quote))
        ; Strip out any CR characters and recur
        ((char=? c #\return)
         (string-loop (read-char in) s in-quote double-quote))
        (else
         (string-loop (read-char in) (string-append s (string c))
                      in-quote double-quote))))))

(define (convert-string-to-number line)
  ;; Given a list of strings and numbers, this will convert
  ;; the strings that are numbers to an actual number
  ;; then return the new list
  (map (lambda (x) (if (and (number? (string->number x))
                            ; Make sure it isn't something line '1234/56'
                            (inexact? (string->number x)))
                       (string->number x)
                       x))
               line))

(define (csv-parse-file filename separator)
  ;; Pulls in a csv file, parses it and returns a list
  ;; of csv lines that are lists themselves
  ;; If a 'cell' is a number, then convert it from a string
  (let ((csv-file (pull-csv filename)))
    (map (lambda (x) (convert-string-to-number x))
         (map (lambda (y) (separate-line y separator)) csv-file))))

;; TODO: Save a list to a CSV file
