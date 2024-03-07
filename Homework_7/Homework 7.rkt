;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Homework 7|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All Problems for Homework 7

;; This assignment asks you to design several functions that employ the
;; following data designs. The functions that you design *must* use list
;; abstraction(s) when appropriate.
;;
;; NOTE #1: Part of the credit for each problem will be based on the choice of
;; list abstractions, so make sure that they are a good match for the problem.
;;
;; NOTE #2: For certain problems, you will have to design helper functions that
;; do not use list abstractions. You should follow the full design recipe
;; (including templates) for all problems.
;;
;; NOTE #3: For every function that you design, follow the "2+ tests and no
;; halloween colors" rule.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Designs (do not modify these)

;; A CallType is one of:
;; - "zoom"
;; - "teams"
;; - "phone"
;; Interpretation: a type of call

(define CT-ZOOM "zoom")
(define CT-TEAMS "teams")
(define CT-PHONE "phone")

(define (calltype-temp ct)
  (cond
    [(string=? ct CT-ZOOM) ...]
    [(string=? ct CT-TEAMS) ...]
    [(string=? ct CT-PHONE) ...]))

(define-struct call [type duration attendees description])
(define-struct mtg [duration attendees description])
(define-struct alone [duration description])

;; An Event is one of:
;; - (make-call CallType PosInt [NEList-of String] String)
;; - (make-mtg PosInt [NEList-of String] String)
;; - (make-alone PosInt String)
;; Interpretation: an event in some period of time, which is either:
;; - A call using some technology, lasting some number of minutes with attendees
;;  (by name), and a description;
;; - An in-person meeting lasting some number of minutes
;;   with attendees (by name) and a description; or
;; - Time spent alone for some number of minutes with a description.

(define E-ZOOM-DOC
  (make-call CT-ZOOM 22 (list "Dr. Zoidberg")
             "Doctor appointment about a stomach ache after some bad seafood :("))
(define E-TEAMS-OH
  (make-call CT-TEAMS 7 (list "Mike" "Tajel")
             "Office hours with my partner to ask clarifying questions about the Design Recipe!"))
(define E-PHONE-SPAM
  (make-call CT-PHONE 1 (list "Unknown")
             "Who calls!? I think it was a scam..."))
;; These are characters from a TV show called "Friends", which was popular in
;; the 90s, which is when many of your instructors grew up.
(define E-MTG-STUDY
  (make-mtg 62 (list "Rachel" "Ross" "Joey" "Phoebe" "Chandler" "Monica")
            "Getting ahead on studying for Exam 2!"))
(define E-MTG-ADVISOR
  (make-mtg 28 (list "Ali") 
            "Meeting with advisor to talk about a combined major"))
(define E-ALONE-LUNCH
  (make-alone 34 "Lunch"))
(define E-ALONE-READING
  (make-alone 25 "The Three-Body Problem isn't going to read itself!"))
(define LOE-1
  (list E-ZOOM-DOC E-ALONE-READING E-PHONE-SPAM
        E-ALONE-LUNCH E-TEAMS-OH E-MTG-ADVISOR E-MTG-STUDY))

(define (e-temp e)
  (cond
    [(call? e)
     (... (calltype-temp (call-type e)) ...
          (call-duration e) ...
          (los-temp (call-attendees e)) ...
          (call-description e) ...)]
    [(mtg? e)
     (... (mtg-duration e) ...
          (los-temp (mtg-attendees e)) ...
          (mtg-description e) ...)]
    [(alone? e)
     (... (alone-duration e) ...
          (alone-description e) ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part A

;; Design a function called weekend that reminds you to rest. The function
;; consumes an argument -- a positive integer -- and produces a list of that
;; many alone events. Each of these is 30mins each, with the description "rest".

;; [TODO] Function design. Use list abstractions when appropriate.

;; weekend : PosInt -> [Listof Events]
;; Produces a list of alone events, given a number, that reminds you to rest. Each of
;; these is 30 minutes, with the description "rest".

(check-expect (weekend 1) (list (make-alone 30 "rest")))
(check-expect (weekend 2) (list (make-alone 30 "rest")
                                (make-alone 30 "rest")))

(define (weekend num)
  (build-list num weekend/rest))

(define (weekend/rest _)
  (make-alone 30 "rest"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part B

;; Design the function small-group that consumes a list of events and only
;; produces those that have fewer than three participants. In all cases,
;; there is an implied attendee (you!) that counts as one person. So,
;; calls and meetings with two or more attendees are *not* small groups.

;; [TODO] Function design. Use list abstractions when appropriate.

;; small-group : (Listof Events) -> (Listof Events)
;; Produces a list of events with fewer than three participants.

(check-expect 
 (small-group (list E-MTG-STUDY E-MTG-ADVISOR))
 (list E-MTG-ADVISOR))
(check-expect
 (small-group (list E-ZOOM-DOC
                    E-ZOOM-DOC
                    E-TEAMS-OH 
                    E-MTG-ADVISOR
                    E-MTG-STUDY))
 (list E-ZOOM-DOC
       E-ZOOM-DOC
       E-MTG-ADVISOR))

(define (small-group loe)
  (filter small-group? loe))

;; small-group? : Event -> Boolean
;; Checks if the event conatins fewer than three participants

(check-expect (small-group? E-ZOOM-DOC) #true)
(check-expect (small-group? E-TEAMS-OH) #false)

(define (small-group? e)
  (cond
    [(call? e)
     (< (small-group?/call (call-attendees e)) 2)]
    [(mtg? e)
     (< (small-group?/mtg (mtg-attendees e)) 2)]
    [(alone? e) #true]))

(define (small-group?/call los)
  (length los))

(define (small-group?/mtg los)
  (length los))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part C

;; Design the function had-lunch? that accepts a list of events and determines
;; if it contains an event whose description contains the word "lunch" 
;; (without regard for upper/lower-case).

;; [TODO] Function design. Use list abstractions when appropriate.

;; had-lunch? : [Listof Events] -> Boolean
;; Determines if a list of events contains an event whose description contains the word "lunch".
(check-expect (had-lunch? (list E-MTG-STUDY)) #false)
(check-expect (had-lunch? LOE-1) #true)

(define (had-lunch? loe)
  (ormap had-lunch?/e loe))

(define (had-lunch?/e e)
  (cond
    [(call? e)
     (string-contains? (string-downcase (call-description e)) "lunch")]
    [(mtg? e)
     (string-contains? (string-downcase (mtg-description e)) "lunch")]
    [(alone? e)
     (string-contains? (string-downcase (alone-description e)) "lunch")]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part D

;; Design the function social-time that accepts a list of events and produces
;; the total minutes spent on calls or meetings.

;; [TODO] Function design. Use list abstractions when appropriate.

;; social-time : [Listof Events] -> PosInt
;; Produces the total minutes spent on calls or meetings.

(check-expect
 (social-time (list E-MTG-ADVISOR
                    E-ALONE-LUNCH
                    E-TEAMS-OH))
 69)

(check-expect
 (social-time LOE-1)
 179)

(define (social-time loe)
  (foldr + 0 (map event-type loe)))

(define (event-type e)
  (cond
    [(call? e)
     (call-duration e)]
    [(mtg? e)
     (mtg-duration e)]
    [(alone? e)
     (alone-duration e)]))
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Part E

;; Design the function anything-but-phone? that accepts a list of events and
;; and produces #true if none of the calls were phone calls. (Zoom and Teams
;; calls are not phone calls.)

;; [TODO] Function design. Use list abstractions when appropriate.

;; anything-but-phone? : [Listof Events] -> Boolean
;; Given a list of events, produces #true if none of the calls were phone calls.
;; (Zoom and Teams calls are not phone calls.)

(check-expect
 (anything-but-phone? (list E-ZOOM-DOC E-TEAMS-OH))
 #true)

(check-expect
 (anything-but-phone? LOE-1)
 #false)

(define (anything-but-phone? loe)
  (andmap anything-but-phone?/e loe))

(define (anything-but-phone?/e e)
  (cond
    [(call? e)
     (not-phone? (call-type e))]
    [(mtg? e) #true]
    [(alone? e) #true]))

(define (not-phone? ct)
  (cond
    [(string=? ct CT-ZOOM) #true]
    [(string=? ct CT-TEAMS) #true]
    [(string=? ct CT-PHONE) #false])) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part F

;; Design the function summary that accepts a list of
;; events and produces a list of textual summaries for each; for example...
;;
;; "Had a Teams call with Mike and Tajel for 7min:
;;  'Office hours with my partner to ask clarifying questions about the Design Recipe!'"
;;
;; "Met with Ali for 28min: 'Meeting with advisor to talk about a combined major'"
;;
;; "Spent 25min: 'The Three-Body Problem isn't going to read itself!'"
;;
;; Your summaries do not have exactly the same format as the examples above. 
;; But, they should contain the same information.

;; [TODO] Function design. Use list abstractions when appropriate.

;; summary : [Listof Events] -> [Listof Strings]
;; Produces a list of textual summaries for each list of events given.

(check-expect
 (summary (list E-ZOOM-DOC))
 (list "Had a zoom call with Dr. Zoidberg for 22min:
'Doctor appointment about a stomach ache after some bad seafood :('"))

(check-expect
 (summary (list E-MTG-ADVISOR
                E-TEAMS-OH))
 (list "Met with Ali for 28min:
'Meeting with advisor to talk about a combined major'"
       "Had a teams call with Mike and Tajel for 7min:
'Office hours with my partner to ask clarifying questions about the Design Recipe!'"))

(define (summary loe)
  (map description loe))

(define (description e)
  (cond
    [(call? e)
     (string-append "Had a " (call-type e) " call with "
                    (names/attendees (call-attendees e))
                    " for " (number->string (call-duration e)) "min:"
                    "\n'" (call-description e) "'")]
    [(mtg? e)
     (string-append "Met with " (names/attendees (mtg-attendees e))
                    " for " (number->string (mtg-duration e)) "min:"
                    "\n'" (mtg-description e)"'")]
    [(alone? e)
     (string-append "Spent " (number->string (alone-duration e)) "min: '"
                    (alone-description e))])) 

(define (names/attendees los)
  (foldr string-append/and (first los) (rest los)))

(define (string-append/and s1 s2)
  (string-append s2 " and " s1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part G

;; Design the function peeps that accepts a list of
;; events and produces an alphabetically sorted list of attendees
;; at all the calls/meetings (including any duplicates).

;; [TODO] Function design. Use list abstractions when appropriate.

;; peeps: [Listof Events] -> [Listof Strings]
;; Produces an alphabetically sorted list of attendees
;; at all the calls/meetings (including any duplicates).

(check-expect
 (peeps (list E-TEAMS-OH E-ZOOM-DOC))
 (list "Dr. Zoidberg"
       "Mike"
       "Tajel"))

(check-expect
 (peeps (list E-MTG-STUDY))
 (list "Chandler"
       "Joey"
       "Monica"
       "Phoebe"
       "Rachel"
       "Ross"))

(define (peeps loe)
  (sort (combine-list loe) alphabetical))


;; combine-list : [Listof Events] -> [Listof Strings]
(define (combine-list loe)
  (foldr append '() (map get-names loe)))

;; get-names : Event -> [Listof Strings]
(define (get-names e)
  (cond
    [(call? e)
     (call-attendees e)]
    [(mtg? e)
     (mtg-attendees e)]
    [(alone? e) '()]))


;; alphabetical : String -> Boolean
(define (alphabetical s1 s2)
  (string<? s1 s2))

