;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; NOTE #1: Feel free to use list abstractions if you like.
;; NOTE #2: The "2+ check-expects and no-halloween-colors" rule applies.

;; Consider the following data definitions for a network (graph) of Subway
;; stations.

(define-struct station [name connections])
;; A Station is a (make-station String [List-of String])
;; Interpretation: The name of a station and the names of stations that directly
;; connect to it.

(define EX-STATION-1 (make-station "Newton Centre" (list "Fenway" "Kenmore")))
(define EX-STATION-2 (make-station "Fenway" (list "Newton Highlands" "Newton Centre")))
(define EX-STATION-3 (make-station "Kenmore" (list "Newton Centre")))
(define EX-STATION-4 (make-station "Newton Highlands" (list "Fenway")))

;; station-template : Station -> ?
(define (station-template s)
  (... (station-name s) ...
       (los-template (station-connections s)) ...))

;; A Subway is a [List-of Station]
;; Interpretation: A list of stations that make a subway network.

(define EX-SUBWAY-1 (list EX-STATION-1 EX-STATION-2 EX-STATION-3 EX-STATION-4))

;; A silly example, but technically fits the data definition.
(define EX-SUBWAY-2 '())

;; Wow, a circuit in a subway! The Moscow Metro actual has a circuit.
(define EX-SUBWAY-3 (list (make-station "A" (list "B" "D"))
                          (make-station "B" (list "C" "A"))
                          (make-station "C" (list "D" "B"))
                          (make-station "D" (list "A" "C"))))

;; subway-template : Subway -> ?
(define (subway-template s)
  (...
   (cond
     [(empty? s) ...]
     [(cons? s) (... (station-template (first s)) ... 
                     (subway-template (rest s)) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part A

;; Design a function called add-edge that adds an edge to a Subway from one
;; station to another *in just one direction*. We have given you the
;; signature, purpose statement, and check-expects, so you only need to
;; write the function body, and potentially a helper function.

;; add-edge : String String Subway -> Subway
;; (add-edge from to station) adds a connection to the subway, if it does not
;; already exist. Assumes that both from and to stations in the Subway.

(check-expect
 (add-edge "Fenway" "Kenmore" EX-SUBWAY-2) EX-SUBWAY-2)

(check-expect
 (add-edge "Fenway" "Kenmore" EX-SUBWAY-1)
 (list EX-STATION-1
       (make-station "Fenway" (list "Kenmore" "Newton Highlands" "Newton Centre"))
       EX-STATION-3
       EX-STATION-4))

(check-expect (add-edge "Kenmore" "Newton Centre" EX-SUBWAY-1) EX-SUBWAY-1)

(check-expect (add-edge "A" "C" EX-SUBWAY-3)
              (list (make-station "A" (list "C" "B" "D"))
                    (make-station "B" (list "C" "A"))
                    (make-station "C" (list "D" "B"))
                    (make-station "D" (list "A" "C"))))

;; Do not alter the signature, purpose, and check-expects written above!

(define (add-edge from to sub)
  (local [;; in-list? : [Listof String] -> Boolean
          ;; Checks if the to station is in the list of station connections.
          (define (in-list? los)
            (cond
              [(empty? los) #false]
              [(cons? los)
               (or
                (string=? to (first los))
                (in-list? (rest los)))]))

          ;; change-station : Station -> Station
          ;; Checks if station name is equal to from station and
          ;; appends the to station to the list of station connections
          ;; if not already in the list. 
          (define (change-station st)
            (if (and
                 (string=? (station-name st) from)
                 (not (in-list? (station-connections st))))
                (make-station (station-name st) (append (list to) (station-connections st)))
                st))]
    (cond
      [(empty? sub) sub]
      [(cons? sub)
       (cons (change-station (first sub))  
             (add-edge from to (rest sub)))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part B

;; We will use the following data definition in this problem:

;; A [List-of-2-or-more X] is one of:
;; - (cons X (cons X '()))
;; - (cons X [List-of-2-or-more X])
;; A list with two or more items.

(define EX-LO2OM-1 (list "Newton Centre" "Fenway"))
(define EX-LO2OM-2 (list "Kenmore" "Landsdowne" "Newton Centre"))

(define (list-of-2-or-more-template lo2om)
  (...
   (cond
     [(empty? (rest (rest lo2om))) 
      (... (first lo2om) ...
           (first (rest lo2om)) ...)]
     [(cons? (rest (rest lo2om)))
      (... (first lo2om) ...
           ;; The line below is the second item in the list. We know it exists
           ;; because (rest (rest lo2om)) has 2+ items. 
           (first (rest lo2om)) ... 
           (list-of-2-or-more-template (rest lo2om)) ...)])))

;; Design a function that consumes a [List-of-2-or-more String], where each 
;; String represents a station on a subway line. The function should produce a 
;; Subway with those stations connected appropriately. i.e.,  each station in
;; the line is directly connected to both the next and previous stations
;; on the line. Here is one example test (you should add more):

;; Hint #1: It helps to begin with a subway where all stations are disconnected.
;; from each other.
;;
;; Hint #2: Design a helper function that creates connections in just one
;; direction (which will likely use add-edge function Part A!). 
;; - Perhaps the disconnected stations could be fed to this helper?
;; - What if you also had the stations listed in the opposition direction too?

;; [TODO] Function design

;; line->subway : [List-of-2-or-more Station] -> Subway
;; Produces a Subway with the inputted stations connected.
;; Each station is direclty connected to both the next and
;; previous stations on the line.

(check-expect
 (line->subway (list "A" "B"))
 (list (make-station "A" (list "B"))
       (make-station "B" (list "A"))))

(check-expect
 (line->subway EX-LO2OM-1)
 (list (make-station "Newton Centre" (list "Fenway"))
       (make-station "Fenway" (list "Newton Centre"))))

(check-expect 
 (line->subway (list "A" "B" "C"))
 (list (make-station "A" (list "B"))
       (make-station "B" (list "A" "C"))
       (make-station "C" (list "B"))))

(check-expect 
 (line->subway EX-LO2OM-2)
 (list (make-station "Kenmore" (list "Landsdowne"))
       (make-station "Landsdowne" (list "Kenmore" "Newton Centre"))
       (make-station "Newton Centre" (list "Landsdowne"))))

(check-expect
 (line->subway (list "A" "B" "C" "D"))
 (list (make-station "A" (list "B"))
       (make-station "B" (list "A" "C"))
       (make-station "C" (list "B" "D"))
       (make-station "D" (list "C"))))

(define (line->subway lo2om)
  (local [;; make-empty-subway : [List-of-2-or-more X] -> Subway
          ;; Creates a subway from the given list of stations,
          ;; but without any connections.
          (define (make-empty-subway lo2om)
            (map
             (λ (st-name) (make-station st-name '()))
             lo2om))

          ;; add-connections : Subway [List-of-2-or-more X] -> Subway
          ;; Adds connections to the subway based on the list of stations,
          ;; but only in one direction. 
          (define (add-connections sub lo2om)
            (cond
              [(empty? (rest (rest lo2om)))
               (add-edge
                (first lo2om)
                (first (rest lo2om)) sub)]
              [(cons? (rest (rest lo2om)))
               (add-connections
                (add-edge (first lo2om) (first (rest lo2om)) sub)
                (rest lo2om))]))]
    (add-connections
     (add-connections (make-empty-subway lo2om) lo2om)
     (reverse lo2om))))
    
              


