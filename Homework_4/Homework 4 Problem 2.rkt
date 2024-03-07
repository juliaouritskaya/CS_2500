;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 4 Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; This problem has a partially-completed data design for representing DNA
;; sequences. A DNA molecule is a long sequence of four nucleotides: adenine,
;; cytosine, guanine, and thymine. (More information here:
;; https://en.wikipedia.org/wiki/DNA.)

(define-struct adenine [rest])
(define-struct guanine [rest])
(define-struct cytosine [rest])
(define-struct thymine [rest])
;; A DNASeq is one of: 
;; - (make-adenine DNASeq)
;; - (make-guanine DNASeq)
;; - (make-cytosine DNASeq)
;; - (make-thymine DNASeq)
;; - "empty sequence"
;; Interpretation: A DNASeq represents a sequence of nucleotides.

;; Part A

;; Write at least three examples of DNASeq. Across all examples, ensure you
;; have an example of all nucleotides.

;; [TODO] Three

(define DNA-SEQ-EMPTY "empty sequence")
(define DNA-SEQ-1
  (make-adenine
   DNA-SEQ-EMPTY))
(define DNA-SEQ-2
  (make-guanine
   DNA-SEQ-1))
(define DNA-SEQ-3
  (make-cytosine
   DNA-SEQ-2))
(define DNA-SEQ-4
  (make-thymine
   DNA-SEQ-3))

;; Part B

;; Write a template for DNASeq.

;; [TODO] Template

(define (dnaseq-temp dna)
  (cond
    [(string? dna) (... dna ...)]
    [(adenine? dna) (... (dnaseq-temp (adenine-rest dna)) ...)]
    [(guanine? dna) (... (dnaseq-temp (guanine-rest dna)) ...)]
    [(cytosine? dna) (... (dnaseq-temp (cytosine-rest dna)) ...)]
    [(thymine? dna) (... (dnaseq-temp (thymine-rest dna)) ...)]))
                   

;; Part C

;; Every  DNA sequence has a complementary sequence, which substitutes
;; As with Ts, Ts with As, Cs with Gs, and Gs with Cs. Design a function
;; to calculate the complement of a DNA sequence.

;; [TODO] Function design recipe
 
;; dnaseq->complement : DNASeq -> DNASeq
;; Calculates the complement of a DNA sequence.
(check-expect (dnaseq->complement DNA-SEQ-1) (make-thymine
                                              DNA-SEQ-EMPTY))
(check-expect (dnaseq->complement DNA-SEQ-2) (make-cytosine
                                              (make-thymine
                                               DNA-SEQ-EMPTY)))
(check-expect (dnaseq->complement DNA-SEQ-3) (make-guanine
                                              (make-cytosine
                                               (make-thymine
                                                DNA-SEQ-EMPTY))))
(check-expect (dnaseq->complement DNA-SEQ-4) (make-adenine
                                              (make-guanine
                                               (make-cytosine
                                                (make-thymine
                                                 DNA-SEQ-EMPTY)))))

(define (dnaseq->complement dna)
  (cond
    [(string? dna) "empty sequence"]
    [(adenine? dna) (make-thymine (dnaseq->complement (adenine-rest dna)))] 
    [(guanine? dna) (make-cytosine (dnaseq->complement (guanine-rest dna)))]
    [(cytosine? dna) (make-guanine (dnaseq->complement (cytosine-rest dna)))]
    [(thymine? dna) (make-adenine (dnaseq->complement (thymine-rest dna)))]))


