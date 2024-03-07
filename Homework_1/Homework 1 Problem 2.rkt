;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 1 Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; You and your friend have been arguing about how best to invest some money.
;; You think you've picked some stocks that give decent gains consistently, but
;; your friend really wants to invest in cryptocurrencies, which she believes
;; have substantially larger gains some good days, but also suffer some losses
;; on some bad days. To settle the debate of which to invest in, you offer to 
;; program a simulation of the two choices.

;; Part A

;; Define a function stock-day which simulates a single day of gain from the
;; stocks you have in mind. Specifically, stock-day should receive as its
;; argument the amount of money you have, and should produce the amount you
;; will have after a 4% gain. For example, (stock-day 1000) should produce 1040.
;; In addition, write three examples. Here is one to get you started:
;;
;; (stock-day 1000) ; produces 1040

;; [TODO] Function definition

;; stock-day : Number -> Number
;; Produces the amount of money you will have after a 4% gain in stocks.

(define (stock-day current-money)
  (* current-money 1.04))

;; [TODO] Examples

(stock-day 1000)
(stock-day 8100)
(stock-day 6199)


;; Part B

;; Define a function crypto-good-day which simulates a single day of gain from
;; cryptocurrencies, assuming it was a good day. Specifically, crypto-good-day
;; should calculate the amount of money you will have after a 10% gain.
;; You must also write three examples of this function.

;; [TODO] Function definition

;; crypto-good-day : Number -> Number
;; Produces the amount of money you will have after a 10% gain in crypto.

(define (crypto-good-day current-money)
  (* current-money 1.1))

;; [TODO] Three examples

(crypto-good-day 3426)
(crypto-good-day 225)
(crypto-good-day 75)

;; Part C

;; Define a function crypto-bad-day which simulates a single day of loss from
;; cryptocurrencies, assuming it was a bad day. Specifically, crypto-bad-day
;; should compute the total amount of money you will have after a –2% loss.
;; For example, if you start with $100 in crypto, after a bad day, you will
;; have $98 left. You must also write three examples for this function.

;; [TODO] Function definition

;; crypto-bad-day : Number -> Number
;; Produces the amount of money you will have after a -2% loss in crypto.

(define (crypto-bad-day current-money)
  (- current-money (* current-money 0.02)))

;; [TODO] Three examples

(crypto-bad-day 100)
(crypto-bad-day 347)
(crypto-bad-day 899)

;; Part D
;;
;; Define a constant STOCK-6-DAYS which is the total amount of money you would
;; have after starting with $1000 and repeatedly investing all of it in stocks
;; six days. You must use the stock-day function.
;;
;; Hint: You can use the value produced by stock-day on the first day as the
;; argument for stock-day on the second day, and so on.

;; [TODO] Define the constant

(define STOCK-6-DAYS
  (stock-day (stock-day (stock-day (stock-day (stock-day (stock-day 10000)))))))

;; Define a constant CRYPTO-6-DAYS which  simulates cryptocurrency trading for
;; 6 days starting with $1,000, alternating crypto-good-day and crypto-bad-day,
;; **starting with crypto-good-day**.

;; [TODO] Define the constant

(define CRYPTO-6-DAYS
  (crypto-bad-day (crypto-good-day (crypto-bad-day (crypto-good-day (crypto-bad-day (crypto-good-day 10000)))))))

;; Part E
;;
;; Now compare the results! Which one seems to have done better?

STOCK-6-DAYS
CRYPTO-6-DAYS

;; [TODO] Write which one seems better? Write it as a comment.

;; STOCK-6-DAYS seems better than CRYPTO-6-DAYS because it makes around $12.59 more.
;; STOCK-6-DAYS grows exponentially larger consistently, suffering no losses, while
;; CRYPTO-6-DAYS alternates between gains and losses. 


