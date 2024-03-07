;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 2 Problem 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;; Part A

;; Write a data definition called CompassDirection that represents a single
;; cardinal or inter-cardinal direction that appears on a compass rose:
;;
;; https://en.wikipedia.org/wiki/Cardinal_direction
;;
;; Ensure you follow all steps of the data design recipe.

;; [TODO] Data design recipe

;; CompassDirection is one of:
;; - "North"
;; - "East"
;; - "South"
;; - "West"
;; - "Northeast"
;; - "Southeast"
;; - "Southwest"
;; - "Northwest"
;; Interpretation: Represents the cardinals and intercardinals on a compass rose.

;; Examples:

(define DIRECTION-NORTH "North")
(define DIRECTION-EAST "East")
(define DIRECTION-SOUTH "South")
(define DIRECTION-WEST "West")
(define DIRECTION-NORTHEAST "Northeast")
(define DIRECTION-SOUTHEAST "Southeast")
(define DIRECTION-SOUTHWEST "Southwest")
(define DIRECTION-NORTHWEST "Northwest")

;; Template:

(define (compass-direction direction)
  (cond
    [(string=? direction "North") ...]
    [(string=? direction "East") ...]
    [(string=? direction "South") ...]
    [(string=? direction "West") ...]
    [(string=? direction "Northeast") ...]
    [(string=? direction "Southeast") ...]
    [(string=? direction "Southwest") ...]
    [(string=? direction "Northwest") ...]))

;; Part B

;; Write a predicate to determine if a CompassDirection is a cardinal
;; direction (and not an inter-cardinal direction).

;; [TODO] Function design recipe

;; cardinal-direction? : CompassDirection -> Boolean
;; Determines if a compass direction is a cardinal direction
;; and not an inter-cardinal direction.

(check-expect (cardinal-direction? "North") #true)
(check-expect (cardinal-direction? "Northeast") #false)
(check-expect (cardinal-direction? "South") #true)

(define (cardinal-direction? direction)
  (cond
    [(string=? direction "North") #true]
    [(string=? direction "East") #true]
    [(string=? direction "South") #true]
    [(string=? direction "West") #true]
    [(string=? direction "Northeast") #false]
    [(string=? direction "Southeast") #false]
    [(string=? direction "Southwest") #false]
    [(string=? direction "Northwest") #false]))
    
;; Part C

;; Write a function that consumes a CompassDirection and produces the opposite
;; direction.
 
;; [TODO]

;; switch-direction : CompassDirection -> CompassDirection
;; Produces the opposite direction.

(check-expect (switch-direction "North") "South")
(check-expect (switch-direction "East") "West")
(check-expect (switch-direction "Southeast") "Northwest")

(define (switch-direction direction)
  (cond
    [(string=? direction "North") "South"]
    [(string=? direction "South") "North"]
    [(string=? direction "East") "West"]
    [(string=? direction "West") "East"]
    [(string=? direction "Northeast") "Southwest"]
    [(string=? direction "Southeast") "Northwest"]
    [(string=? direction "Southwest") "Northeast"]
    [(string=? direction "Northwest") "Southeast"]))



