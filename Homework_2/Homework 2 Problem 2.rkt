;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 2 Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; Part A

;; The VIA Rail Canadian is a sleeper train that runs from Vancouver to Toronto.
;; It is also featured on the Canadian $10 bill. Here is a brochure:
;;
;; https://www.viarail.ca/sites/all/files/media/pdfs/111139179-1-Sales-tools_Anglais_WEB.pdf
;;
;; Look up the list of stops the Canadian makes, and write a data definition
;; called CanadianTrainStop that can represent any of these stops.
;; Ensure you follow _all_ steps of the data design recipe.

;; [TODO] Data design recipe

;; A CanadianTrainStop is one of:
;; - "Vancouver"
;; - "Jasper"
;; - "Kamloops"
;; - "Edmonton"
;; - "Saskatoon"
;; - "Winnipeg"
;; - "Sioux Lookout"
;; - "Sudbury Jct."
;; - "Toronto"
;; Interpretation: The list of stops the Canadian makes.

;; Examples:

(define STOP1-Vancouver "Vancouver")
(define STOP2-Jasper "Jasper")
(define STOP3-Kamloops "Kamloops")
(define STOP4-Edmonton "Edmontoon")
(define STOP5-Saskatoon "Saskatoon")
(define STOP6-Winnipeg "Winnipeg")
(define STOP7-SiouxLookout "Sioux Lookout")
(define STOP8-SudburyJct. "Sudbury Jct.")
(define STOP9-Toronto "Toronto")


;; Template:

(define (canadian-train-stop-temp stop)
  (cond
    [(string=? stop "Vancouver") ...]
    [(string=? stop "Kamloops") ...]
    [(string=? stop "Jasper") ...]
    [(string=? stop "Edmontoon") ...]
    [(string=? stop "Saskatoon") ...]
    [(string=? stop "Winnipeg") ...]
    [(string=? stop "Sioux Lookout") ...]
    [(string=? stop "Sudbury Jct.") ...]
    [(string=? stop "Toronto") ...]))
   

;; Part B

;; Design a function called province to determine the province in
;; which a stop is located. Ensure that you *strictly* follow the design
;; recipe.

;; [TODO] Function design recipe

;; province : CanadianTrainStop -> String
;; Determines the province in which a stop is located.

(check-expect (province "Vancouver") "British Columbia")
(check-expect (province "Toronto") "Ontario")
(check-expect (province "Winnipeg") "Manitoba")

(define (province stop)
  (cond
    [(string=? stop "Vancouver") "British Columbia"]
    [(string=? stop "Kamloops") "British Columbia"]
    [(string=? stop "Jasper") "Alberta"]
    [(string=? stop "Edmontoon") "Alberta"]
    [(string=? stop "Saskatoon") "Saskatchewan"]
    [(string=? stop "Winnipeg") "Manitoba"]
    [(string=? stop "Sioux Lookout") "Ontario"]
    [(string=? stop "Sudbury Jct.") "Ontario"]
    [(string=? stop "Toronto") "Ontario"]))



;; Part C

;; Some stops on the Canadian connect to other train lines. Write a predicate
;; called can-transfer? that produces #true when a stop has an available
;; transfer to another line. You must follow all steps of the design recipe.
;; However, you may shorten your function definition if you find it convenient
;; to do so.

;; [TODO] Function design recipe

;; can-transfer? : CanadianTrainStop -> Boolean
;; Determines if a stop has an available transfer to another line.

(check-expect (can-transfer? "Jasper") #true)
(check-expect (can-transfer? "Winnipeg") #true)
(check-expect (can-transfer? "Saskatoon") #false)

(define (can-transfer? stop)
  (if
   (or
    (string=? stop "Jasper")
    (string=? stop "Winnipeg")
    (string=? stop "Toronto"))
   #true
   #false))


