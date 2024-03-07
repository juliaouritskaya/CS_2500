;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exam 1 Problem 7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 7
;;
;; *This is a programming problem that you should do in DrRacket.*
;;
;; You have been tasked with building a web-based takeout menu for a pizzeria.
;; The pizzeria sells pizzas and soft drinks. Every pizza has a name
;; (e.g., "margherita" or "pepperoni"), and size (small, medium, or large).
;; Soft drinks only have a name (e.g., "Pepsi" or "Harmony Springs Cola").
;;
;; Design a data definition that represents a single menu item for the pizzeria.
;; Ensure you follow the entire data design recipe for all data that you design.

;; [TODO] Data designs

;; A Size is one of:
;; - "small"
;; - "medium"
;; - "large"
;; Interpretation: reprsents the size of a pizza.

;; Examples:
(define SIZE-SMALL "small")
(define SIZE-MEDIUM "medium")
(define SIZE-LARGE "large")

;; Template:
(define (size-temp s)
  (...
   (cond
     [(string=? s SIZE-SMALL) ...]
     [(string=? s SIZE-MEDIUM) ...]
     [(string=? s SIZE-LARGE) ...])))

(define-struct pizza [name size])
;; A Pizza is a (make-pizza String Size)
;; - name is the name of the pizza (e.g., "margherita" or "pepperoni")
;; - size is the Size of the pizza (small, medium, or large)
;; Interpretation: represents a pizza of name and specific size

;; Examples:
(define PIZZA-1 (make-pizza "margherita" SIZE-SMALL))
(define PIZZA-2 (make-pizza "pepperoni" SIZE-MEDIUM))
(define PIZZA-3 (make-pizza "cheese" SIZE-LARGE))

;; Template:
(define (pizza-temp p)
  (... (pizza-name p) ...
       (size-temp (pizza-size p)) ...))

(define-struct softdrink [name])
;; A SoftDrink is a (make-softdrink String)
;; - name is the name of the soft drink
;; Interpretation: represents a soft drink with a name (e.g., "Pepsi" or "Harmony Springs Cola")

;; Examples:
(define SOFTDRINK-1 (make-softdrink "Pepsi"))
(define SOFTDRINK-2 (make-softdrink "Harmony Springs Cola"))
(define SOFTDRINK-3 (make-softdrink "Sprite"))

;; Template:
(define (softdrink-temp sd)
  (... (softdrink-name sd) ...))

;; A MenuItem (MI) is one of:
;; - Pizza
;; - SoftDrinks
;; Interpretation: represents a single menu item for the pizzeria

;; Examples:
(define MI-1 PIZZA-1)
(define MI-2 PIZZA-2)
(define MI-3 PIZZA-3)
(define MI-4 SOFTDRINK-1)
(define MI-5 SOFTDRINK-2)
(define MI-6 SOFTDRINK-3)

;; Template:
(define (menuitem-temp mi)
  (...
   (cond
     [(pizza? mi) ...
      (pizza-name mi) ...
      (size-temp (pizza-size mi)) ...]
     [(softdrink? mi) ...
      (softdrink-name mi) ...])))













