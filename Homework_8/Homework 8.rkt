;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introduction

;; In this assignment, you will continue working on Spelling Bee (Homework 3).
;; You will rely on several concepts and techniques that you've learned since
;; Homework 3, to build a version of Spelling Bee that is significantly closer
;; to the real game. In particular, you will:
;;
;; 1. Move to a seven-letter Spelling Bee, instead of a five-letter game,
;;
;; 2. Implement scoring,
;;
;; 3. Support the backspace / delete key, so that players can correct their
;;    word, and
;;
;; 4. Check that the entered word is in a dictionary.

;; NOTE #1: Follow the "no halloween colors" and 2+ check-expects rule for
;; all function designs.
;;
;; NOTE #2: In the original Spelling Bee, we restricted you from using certain
;; functions. For this assignment, the only restricted functions are those
;; in the class style guide.
;;
;; NOTE #3: Despite having fewer restrictions, we still expect good program
;; design. For example, lists are the appropiate type of data to represent
;; scored words and available letters, and this assignment asks you to update
;; your program to use lists. It is possible to immediately convert a 
;; [List-of 1String] into a String. But, that is not the approach we want you
;; to take. We want you to use list abstractions when possible.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 0: Meet With Your Partner

;; You must do this assignment with your assigned partner. However, since you
;; did the previous stage alone, that means you have multiple implementations to
;; Spelling Bee to use as a starting point. Which one will you use? Making that
;; decision is part of the assignment.
;;
;; Note: If neither you nor your partner did well on HW3, we strongly encourage
;; you to use our HW3 sample solution as a starting point.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 1: Introducing Lists
;;
;; When we worked on HW3, we did not know about lists, which are fundamental to
;; good program design. Instead, we played tricks with strings, such as using
;; newlines to separate found words. Now that we are familiar with lists, we
;; are going to modify Spelling Bee to use them in two places:
;;
;; a. Revise your data definition for Letters to either:
;;
;;    - Represent the available letters as an [NE-List-of 1String], or
;;
;;    - Represent the non-center letters as a [List-of 1String]
;;
;; b. Revise your World data definition to represent the list of words found
;;    as a [List-of String]. Thus you should no longer use "\n" in your
;;    examples of World.
;;

;; [TODO] Revise World and Letters as described above.

;; A [NEListof 1String] is one of:
;; - (cons 1String '())
;; - (cons 1String [NEListof 1String])
;; Interpretation: a non-empty list of 1Strings

(define NELOS-0 (list "a"))
(define NELOS-1 (list "o" "a" "k" "t"))
(define NELOS-2 (list "a" "c" "k" "e" "d" "p"))

(define (nelos-temp nelon)
  (...
   (cond
     [(empty? (rest nelon)) ...
      (first nelon) ...]
     [(cons? (rest nelon)) ...
      (first nelon) ...
      (nelos-temp  (rest nelon)) ...])))

(define-struct letters [required available])

;; Letters is a (make-letters 1String [NEListof 1String])
;; - required is the required letter
;; - available is the list of available letters
;; Interpretation: a set of letters that are available for the simplified Spelling Bee
;; with a required letter

(define LETTERS-1 (make-letters "s" NELOS-1))
(define LETTERS-2 (make-letters "t" NELOS-2))

(define (letters-temp lt)
  (... (letters-required lt) ...
       (nelos-temp (letters-available lt)) ...))
   
;; A [Listof String] is one of:
;; - '()
;; - (cons String [Listof String])
;; Interpretation: a list of strings

(define LOS-0 '())
(define LOS-1 (list "soak"))
(define LOS-2 (list "rake" "ackee"))

(define (los-temp los)
  (...
   (cond
     [(empty? los) ...]
     [(cons? los) ...
      (first los) ...
      (los-temp (rest los) ...)])))

(define-struct world [letters partword doneword score])

;; A World is a (make-world Letters String [Listof String] Nat)
;; where Letters is the set of available letters
;; partword is the partial word the player has entered
;; doneword is a list of the finished words with the required letter the player has entered
;; score is the combined score of the donewords
;; Interpretation: A world with an image of the Spelling Bee game with the available letters,
;; the partial word, a list of the finished words, and a score for the game. 

;; Examples:
(define WORLD-1 (make-world LETTERS-1 "" LOS-1 1))
(define WORLD-2 (make-world LETTERS-2 "take" LOS-2 3))

;; Template:
(define (world-temp w)
  (... (letters-temp (world-letters w)) ... 
       (world-partword w) ...
       (los-temp (world-doneword w)) ...
       (world-score w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 2: More Letters
;;
;; The update that you made in Step 1 should allow your program to support any
;; number of available letters (though you need at least one letter at the
;; center). However, your old examples use exactly five letters.
;;
;; 1. Construct new examples and check-expects that have varying numbers of
;;    available letters.
;;
;;  2. Modify letters->image so that it either:
;;
;;     - Assumes that there are exactly seven letters (i.e., as in real
;;       Spelling Bee), or
;;
;;     - Supports any number of available letters (this is not required)

;; [TODO] New examples and check-expects with varying numbers of available
;; letters.

;; (check NELOS examples).

;; [TODO] Update letters->image to display seven letters (or optionally,
;; any number of letters)

(require 2htdp/image)
(require 2htdp/universe)

(define IMAGE-TEXT-SIZE 20)
(define IMAGE-COLOR "black")
(define BLANK (text "" IMAGE-TEXT-SIZE IMAGE-COLOR))
(define BACKGROUND (rectangle 500 400 "outline" "black"))

;; required-letter->image : 1String -> Image
;; Displays a single letter as an image.

(check-expect
 (required-letter->image "a")
 (overlay (text "a" 20 "black")
          (circle 18 "solid" "yellow")))

(check-expect
 (required-letter->image "z")
 (overlay (text "z" 20 "black")
          (circle 18 "solid" "yellow")))

(define (required-letter->image l)
  (overlay (text l 20 "black")
           (circle 18 "solid" "yellow")))

;; available-letter->image : 1String -> Image
;; Displays a single letter as an image.

(check-expect
 (available-letter->image "a")
 (overlay (text "a" 20 "white")
          (circle 18 "solid" "black")))

(check-expect
 (available-letter->image "z")
 (overlay (text "z" 20 "white")
          (circle 18 "solid" "black")))

(define (available-letter->image l)
  (overlay (text l 20 "white")
           (circle 18 "solid" "black")))

;; letters->image : Letters -> Image
;; Displays a Letters.

(check-expect
 (letters->image (make-letters "a" (list "b" "c")))
 (beside (overlay (text "a" 20 "black")
                  (circle 18 "solid" "yellow"))
         (overlay (text "b" 20 "white")
                  (circle 18 "solid" "black"))
         (overlay (text "c" 20 "white")
                  (circle 18 "solid" "black"))))

(check-expect
 (letters->image (make-letters "x" (list "y" "z" "w" "t")))
 (beside (overlay (text "x" 20 "black")
                  (circle 18 "solid" "yellow"))
         (overlay (text "y" 20 "white")
                  (circle 18 "solid" "black"))
         (overlay (text "z" 20 "white")
                  (circle 18 "solid" "black"))
         (overlay (text "w" 20 "white")
                  (circle 18 "solid" "black"))
         (overlay (text "t" 20 "white")
                  (circle 18 "solid" "black"))))

(define (letters->image lt)
  (beside (required-letter->image (letters-required lt))
          (foldr
           (λ (l i) (beside (available-letter->image l) i))
           BLANK
           (letters-available lt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 3: The Backspace/Delete Key
;;
;; Update your program so that when the player presses Backspace (Windows) or
;; Delete (Mac), the game clears the last letter that they entered. The special
;; string "\b" stands for delete/backspace.
;;
;; Note: Ensure your program is "well-behaved" when the player presses
;; delete/backspace and there are no characters to delete.

;; [TODO] Revise your program to support for backspace/delete.

;; key-pressed : World KeyEvent -> World
;; Produce a new World in reaction to a key-press.

(check-expect (key-pressed WORLD-1 "r") WORLD-1)
(check-expect (key-pressed WORLD-2 "e") (make-world LETTERS-2 "takee" LOS-2 3))

(check-expect (key-pressed WORLD-1 "\r") WORLD-1)
(check-expect (key-pressed WORLD-2 "\r")
              (make-world LETTERS-2 ""
                          (append (cons (string-append "take" "") LOS-2))
                          4)) 

(check-expect (key-pressed WORLD-1 "\b") WORLD-1)
(check-expect (key-pressed WORLD-2 "\b") (make-world LETTERS-2 "tak" LOS-2 3))

(define (key-pressed w ke)
  (cond
    [(letter? (world-letters w) ke)
     (make-world (world-letters w)
                 (string-append (world-partword w) ke)
                 (world-doneword w)
                 (world-score w))]  
    [(string=? ke "\r")
     (if (and (doneword? w) (three-checks w (world-partword w)))
         (make-world (world-letters w)
                     ""
                     (append (cons (string-append (world-partword w) "") (world-doneword w)))
                     (+ (world-score w) (scoring (world-partword w))))
         w)]
    [(string=? ke "\b")
     (if (length? w)
         (make-world (world-letters w)
                     (delete (world-partword w))
                     (world-doneword w)
                     (world-score w))
         w)]
    [else w]))

;; letter? : Letters 1String -> Boolean
;; Check if a letter is available.

(check-expect (letter? LETTERS-1 "z") #false)
(check-expect (letter? LETTERS-2 "d") #true)

(define (letter? lt s)
  (or (string-contains? s (letters-required lt))
      (string-contains? s (letters->string (letters-available lt)))))

;; letters->string : [NEListof 1String] -> String
;; Converts the list of available letters into a string.

(check-expect (letters->string NELOS-0) "a")
(check-expect (letters->string NELOS-1) "oakt")
(check-expect (letters->string NELOS-2) "ackedp")

(define (letters->string nelos)
  (cond
    [(empty? (rest nelos)) (first nelos)]
    [(cons? (rest nelos))
     (foldl append-letters (first nelos) (rest nelos))]))

;; append-letters : String String -> String
;; Appends the two given 1Strings, placing the second string before the first.

(check-expect (append-letters "a" "b") "ba")
(check-expect (append-letters "x" "y") "yx")

(define (append-letters s1 s2)
  (string-append s2 s1))

;; doneword? : World -> Boolean
;; Checks if the partial word is complete.

(check-expect (doneword? WORLD-1) #false)
(check-expect (doneword? WORLD-2) #true)

(define (doneword? w)
  (string-contains? (letters-required (world-letters w))
                    (world-partword w)))

;; length? : World -> Boolean
;; Checks if the length of the partial word is greater than 0.

(check-expect (length? WORLD-1) #false)
(check-expect (length? WORLD-2) #true)

(define (length? w)
  (> (string-length (world-partword w)) 0))

;; delete : String -> String
;; Deletes the last letter from a string.

(check-expect (delete "hello") "hell")
(check-expect (delete "goodbye") "goodby")

(define (delete s)
  (substring s 0 (- (string-length s) 1)))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 4: More Checks: Duplicate Words, 4+ Letter-Words, Dictionary Word
;;
;; Revise your program to ensure that the word entered by the player is:
;; 1. An available letter (already done in HW3),
;; 2. Contains the center letter (already done in HW3),
;; 3. A dictionary word,
;; 4. At least four letters long, and
;; 5. Is not a duplicate of an already entered word.
;;
;; We've given you a file called words.txt, which you can use as a dictionary.
;; It is not a comprehensive dictionary, but has roughly 50,000 words. Every
;; line of the file is a word, and they are arranged in alphabetical order
;; (technically, lexicographic order), if W1 appears before W2 in the file,
;; then (string<? W1 W2) is true.
;;
;; Suggestion #1: you can use the read-lines function in the 2htdp/batch-io
;; library to read the dictionary to a constant.
;;
;; Suggestion #2: It is very difficult to work with a list of 50,000 words.
;; So, to get started we recommend defining a small dictionary of words, and
;; then replacing it the the code that reads words from the file. E.g.,

(define LITTLE-DICTIONARY (list "explain" "plain" "nail" "lap"))

;; [TODO] Revise your program to implement the five checks above.

(require 2htdp/batch-io)

;; three-checks : World String -> Boolean
;; Moves the partword to the list of donewords if the word passes the tests.

(check-expect (three-checks WORLD-1 "oak") #false)
(check-expect (three-checks WORLD-2 "ackees") #true)

(define (three-checks w s)
  (and
   (>= (string-length s) 4)
   (andmap (λ (x) (not (string=? s x))) (world-doneword w))
   (ormap (λ (x) (string=? s x)) (read-lines "words.txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 5: Scoring a Game
;;
;; Finally, revise your program to display the current score. The score for
;; each word is:
;;
;; 1. One point for a four-letter word,
;;
;; 2. An additional point for every additional letter beyond the first four, and
;;
;; 3. An additional seven bonus points for using all seven letters.

;; [TODO] Revise your program to support scoring.

;; scoring : String -> Number
;; Scrores the game: one point for a four-letter word
;; an additional point for every additional letter beyond the first four, and
;; an additional seven bonus points for using all seven letters.

(check-expect (scoring "hope") 1)
(check-expect (scoring "hello") 2)
(check-expect (scoring "scoring") 8)

(define (scoring s)
  (cond
    [(= (string-length s) 4) 1]
    [(<= (string-length s) 6)
     (+ (- (string-length s) 4) 1)]
    [(= (string-length s) 7) 8]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (play w)
  (big-bang w
    [to-draw world->image]
    [on-key key-pressed]))

;; world->image : World -> Image
;; Displays the world as an image.

(check-expect (world->image WORLD-1)
              (overlay
               (above (letters->image LETTERS-1)
                      (text "" 18 "black")
                      (text (doneword->string LOS-1) 18 "black")
                      (text (number->string 1) 18 "red"))
               BACKGROUND))

(check-expect (world->image WORLD-2)
              (overlay
               (above (letters->image LETTERS-2)
                      (text "take" 18 "black")
                      (text (doneword->string LOS-2) 18 "black")
                      (text (number->string 3) 18 "red"))
               BACKGROUND))

(define (world->image w)
  (overlay
   (above (letters->image (world-letters w))
          (text (world-partword w) 18 "black")
          (text (doneword->string (world-doneword w)) 18 "black")
          (text (number->string (world-score w)) 18 "red"))
   BACKGROUND))

;; doneword->string : [Listof String] -> String
;; Converts the list of donewords into a string.

(check-expect (doneword->string LOS-0) "")
(check-expect (doneword->string LOS-1) "soak")
(check-expect (doneword->string LOS-2) "ackee\nrake")

(define (doneword->string los)
  (cond
    [(empty? los) ""] 
    [(cons? los) 
     (foldl append-words (first los) (rest los))]))

;; append-words : String String -> String
;; Appends two donewords.

(check-expect (append-words "hello" "hey") "hello\nhey")
(check-expect (append-words "hi" "bye") "hi\nbye")

(define (append-words s1 s2)
  (string-append s1 "\n" s2)) 

