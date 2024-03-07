;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Template copy 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5

;; You are running a car dealership, which seems to be a hot business these
;; days. Design data to represent a single vehicle in the dealership. You
;; need to track the vehicle's manufacturer (brand), the year it was made,
;; the price, whether or not it is new, and whether it is electric,
;; hybrid-electric, or runs on gasoline (petrol).


;; [TODO] Data design. Your examples should include new and used cars, all types 
;; of powertrains, and a variety of other reasonable values for other aspects of
;; a vehicle.

; Fuel is one of:
; - "Electric"
; - "Hybrid-Electric"
; - "Gasoline"
; Interpretation: represents the type of fuel a car runs on

; Examples:
(define ELECTRIC "Electric")
(define H-ElECTRIC "Hybrid-Electric")
(define GASOLINE "Gasoline")

; Template:
(define (fuel-temp f)
  (...
   (cond
     [(string=? "Electric" f) ... ]
     [(string=? "Hybrid-Electric" f) ... ]
     [(string=? "Gasoline" f) ... ])))

(define-struct vehicle [brand year price new fuel])

; A Vehicle is a (make-vehicle String Nat Nat Boolean Fuel)
; - brand is the manufacturer of the vehicle
; - year is the year the vehicle was made
; - price is the price of the vehicle
; - new is whether or not the vehicle is new 
; - Fuel
; - Interpretation: represents a single vehicle in a car delearship

; Examples:

(define CAR1 (make-vehicle "Acura" 2020 10000 #true ELECTRIC))
(define CAR2 (make-vehicle "BMW" 2015 200000 #true H-ElECTRIC))
(define CAR3 (make-vehicle "Chevrolet" 2000 5000 #false GASOLINE))
(define CAR4 (make-vehicle "Dodge" 1995 600 #false GASOLINE))

; Template: 
(define (vehicle-temp v)
  (... (vehicle-brand v) ...
       (vehicle-year v) ...
       (vehicle-price v) ...
       (vehicle-new v) ...
       (fuel-temp (vehicle-fuel v)) ...))


