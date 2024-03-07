;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exam 1 Problem 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 8
;;
;; *This is a programming problem that you should do in DrRacket.*
;;
;; Consider the following data definition and interpretation:

(define-struct star [name mass])
(define-struct planet [name mass closer])

;; A SolarSystem is one of:
;; - (make-star String Number)
;; - (make-planet String Number SolarSystem)
;; Interpretation: A solar system has a star, and some number of planets
;; orbiting that star.
;; A SolarSystem is either:
;; - The star of a solar system, with its name and mass in Earth masses.
;;   (The mass of the Earth is 1 Earth mass.)
;; - A planet with name and mass (in Earth masses) that orbits farther
;;   than the portion of the solar system that is closer.


;; Part A

;; Write four examples of a SolarSystem. You do *not* have to get names of
;; planets or their masses correct. Feel free to make them up.

;; [TODO] Four examples
(define SOLARSYSTEM-1 (make-star "Vega" 4))
(define SOLARSYSTEM-2 (make-planet "Uranus" 5 SOLARSYSTEM-1))
(define SOLARSYSTEM-3 (make-planet "Jupiter" 6 SOLARSYSTEM-2))
(define SOLARSYSTEM-4 (make-planet "Pluto" 2 SOLARSYSTEM-3))


;; Part B

;; Write the template for a SolarSystem.

;; [TODO] Template
(define (solarsystem-temp ss)
  (...
   (cond
     [(star? ss) ...
      (star-name ss) ...
      (star-mass ss) ...]
     [(planet? ss) ...
      (planet-name ss) ...
      (planet-mass ss)
      (solarsystem-temp (planet-closer ss)) ...])))


;; Part C

;; Design a function called solar-system-star-name
;; that produces the name of the star in a solar system.

;; [TODO] Function design

;; solar-system-star-name : SolarSystem -> String
;; Produces the name of the star in a given solar system.

(check-expect (solar-system-star-name SOLARSYSTEM-1) "Vega")
(check-expect (solar-system-star-name SOLARSYSTEM-2) "Vega")
(check-expect (solar-system-star-name SOLARSYSTEM-3) "Vega")
(check-expect (solar-system-star-name SOLARSYSTEM-4) "Vega")

(define (solar-system-star-name ss)
  (cond
    [(star? ss) (star-name ss)]
    [(planet? ss) (solar-system-star-name (planet-closer ss))]))



