;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 4 Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Consider the following structure definitions:

(define-struct guitar [brand-name color electric?])
(define-struct drum-kit [brand-name electric?])
(define-struct saxophone [brand-name])
(define-struct piano [brand-name])

;; Part A

;; Design four data types called Guitar, DrumKit, Saxophone, and Piano: one for 
;; each structure. Ensure you complete all steps of the data design recipe for 
;; all four data types.

;; [TODO] Four complete data designs

;; A Guitar is a (make-guitar String String Boolean)
;; - brand-name is the brand name of the guitar
;; - color is the color of the guitar
;; - electric? checks whether it is an electric guitar
;; Interpretation: represents a guitar

;; Examples:
(define GUITAR-1 (make-guitar "Gibson" "Red" #false))
(define GUITAR-2 (make-guitar "Yamaha" "Brown" #false))
(define GUITAR-3 (make-guitar "Fender" "Blue" #true))

;; Template:
(define (guitar-temp g)
  (... (guitar-brand-name g) ...
       (guitar-color g) ...
       (guitar-electric? g) ...))

;; A DrumKit is a (make-drum-kit String Boolean)
;; - brand-name is the brand name of the drum-kit
;; - electric? checks whether it is an electric drum-kit
;; Interpretation: represents a drum-kit

;; Examples:
(define DRUM-KIT-1 (make-drum-kit "Tama" #true))
(define DRUM-KIT-2 (make-drum-kit "Sonor" #false))
(define DRUM-KIT-3 (make-drum-kit "Canopus" #true))

;; Template:
(define (drum-kit-temp d)
  (... (drum-kit-brand-name d) ...
       (drum-kit-electric? d) ...))

;; A Saxophone is a (make-saxophone String)
;; - brand-name is the brand name of the saxophone
;; Interpretation: represents a saxophone

;; Examples:
(define SAXOPHONE-1 (make-saxophone "Selmer Paris"))
(define SAXOPHONE-2 (make-saxophone "Elkhart"))
(define SAXOPHONE-3 (make-saxophone "Cannonball Saxophones"))

;; Template:
(define (saxophone-temp s)
  (... (saxophone-brand-name s) ...))

;; A Piano is a (make-piano String)
;; - brand-name is the brand name of the piano
;; Interpretation: represents a piano

;; Examples:
(define PIANO-1 (make-piano "Steinway & Sons"))
(define PIANO-2 (make-piano "FAZIOLI"))
(define PIANO-3 (make-piano "Grotrian"))

;; Template:
(define (piano-temp p)
  (... (piano-brand-name p) ...))

;; Part B

;; Design a data type called Instrument, which can represent any one of the
;; four instruments defined above.

;; [TODO] Data design recipe

;; An Instrument is one of:
;; - Guitar
;; - DrumKit
;; - Saxophone
;; - Piano
;; Interpretation: represents any one of the four instruments

;; Examples:
(define INSTRUMENT-1 GUITAR-1)
(define INSTRUMENT-2 DRUM-KIT-2)
(define INSTRUMENT-3 SAXOPHONE-1)
(define INSTRUMENT-4 PIANO-1)
(define INSTRUMENT-5 GUITAR-3)

;; Template:
(define (instrument-temp i)
  (cond
    [(guitar? i) (guitar-temp i)]
    [(drum-kit? i) (drum-kit-temp i)]
    [(saxophone? i) (saxophone-temp i)]
    [(piano? i) (piano-temp i)]))


;; Part C

;; Design a data type called Band, which may have 1, 2, or 3 instruments.
;; The Band data type should hold information about all the instruments in
;; the band.

;; [TODO] Data design recipe

(define-struct oneband [instrument1])
(define-struct twoband [instrument1 instrument2])
(define-struct threeband [instrument1 instrument2 instrument3])

;; A Band is one of:
;; (make-oneband Instrument)
;; (make-twoband Instrument Instrument)
;; (make-threeband Instrument Instrument Instrument)
;; Interpretation: a band with either 1, 2, or 3 instruments

;; Examples:
(define BAND-1 (make-oneband INSTRUMENT-1))
(define BAND-2 (make-twoband INSTRUMENT-1 INSTRUMENT-2))
(define BAND-3 (make-threeband INSTRUMENT-3 INSTRUMENT-4 INSTRUMENT-5))

;; Template:
(define (band-temp b)
  (cond
    [(oneband? b) (... (instrument-temp (oneband-instrument1 b)) ...)]
    [(twoband? b) (... (instrument-temp (twoband-instrument1 b)) ...
                       (instrument-temp (twoband-instrument2 b)))]
    [(threeband? b) (... (instrument-temp (threeband-instrument1 b)) ...
                         (instrument-temp (threeband-instrument2 b)) ...
                         (instrument-temp (threeband-instrument3 b)) ...)]))
;; Part D

;; Design a function that takes a band and produces another band that is
;; identical, except that all guitars and drums become electric!

;; [TODO] Function design

;; band->electricband : Band -> Band
;; Produces a band that is identical, except that
;; all the guitars and drums become electric.
(check-expect (band->electricband BAND-1) (make-oneband (make-guitar "Gibson" "Red" #true)))
(check-expect (band->electricband BAND-2) (make-twoband (make-guitar "Gibson" "Red" #true)
                                                        (make-drum-kit "Sonor" #true)))
(check-expect (band->electricband BAND-3) BAND-3)
 
(define (band->electricband b)
  (cond
    [(oneband? b) (make-oneband (instrument->einstrument (oneband-instrument1 b)))]
    [(twoband? b) (make-twoband (instrument->einstrument (twoband-instrument1 b))
                                (instrument->einstrument (twoband-instrument2 b)))]
    [(threeband? b) (make-threeband (instrument->einstrument (threeband-instrument1 b))
                                    (instrument->einstrument (threeband-instrument2 b))
                                    (instrument->einstrument (threeband-instrument3 b)))]))

;; guitar->eguitar: Guitar -> Guitar
;; Produces an electric guitar.
(check-expect (guitar->eguitar GUITAR-1) (make-guitar "Gibson" "Red" #true))
(check-expect (guitar->eguitar GUITAR-2) (make-guitar "Yamaha" "Brown" #true))
(check-expect (guitar->eguitar GUITAR-3) GUITAR-3)

(define (guitar->eguitar g)
  (make-guitar
   (guitar-brand-name g)
   (guitar-color g)
   #true))
   
;; drum-kit->edrum-kit: Drum -> Drum
;; Produces an electric drum-kit.
(check-expect (drum-kit->edrum-kit DRUM-KIT-1) DRUM-KIT-1)
(check-expect (drum-kit->edrum-kit DRUM-KIT-2) (make-drum-kit "Sonor" #true))
(check-expect (drum-kit->edrum-kit DRUM-KIT-3) DRUM-KIT-3)

(define (drum-kit->edrum-kit d)
  (make-drum-kit
   (drum-kit-brand-name d)
   #true))

;; instrument->einstrument : Instrument -> Instrument
;; Produces an electric guitar or an electric drum-kit.
(check-expect (instrument->einstrument INSTRUMENT-1) (make-guitar "Gibson" "Red" #true))
(check-expect (instrument->einstrument INSTRUMENT-2) (make-drum-kit "Sonor" #true))
(check-expect (instrument->einstrument INSTRUMENT-3) SAXOPHONE-1)

(define (instrument->einstrument i)
  (cond
    [(guitar? i) (guitar->eguitar i)]
    [(drum-kit? i) (drum-kit->edrum-kit i)]
    [(saxophone? i) i]
    [(piano? i) i]))


 


