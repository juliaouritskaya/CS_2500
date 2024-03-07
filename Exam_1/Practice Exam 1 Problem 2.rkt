;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Practice Exam 1 Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Exam 1, Problem 2 ==

; TODO #1: design the function full-name that accepts a person's
; first name (e.g., "Grace"), last name (e.g., "Hopper"), and
; desired ordering (either first name then last name, or last name
; then first name), and produces the appropriate full name (e.g.,
; "Grace Hopper" or "Hopper, Grace").

;; full-name : String String Boolean -> String
;; Produces a full name based upon the desired ordering.
;; (#true = first name then last name)
;; (#false = last name then first name)
(check-expect (full-name "Grace" "Hopper" #true) "Grace Hopper")
(check-expect (full-name "Grace" "Hopper" #false) "Hopper, Grace")

(define (full-name first last first-first?)
  (if first-first?
      (string-append first " " last)
      (string-append last ", " first)))


