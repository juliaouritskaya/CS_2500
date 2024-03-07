;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exam 2 Review Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Exam 2, Problem 2 ==

; Consider the following (familiar) data definitions and examples...


(define-struct file [name size])

; A File is a (make-file String Nat)
; - name is the name of the file (including extension)
; - size is the size of the file in bytes

(define FILE-CV (make-file "cv.pdf" 466000))
(define FILE-HELLO (make-file "hello.rkt" 888))
(define FILE-PIC (make-file "pic.jpg" 968000))
(define FILE-SCHED (make-file "schedule.pdf" 288000))
(define FILE-P1 (make-file "p1.sql" 348))
(define FILE-P2 (make-file "p2.sql" 265))

(define (file-temp f)
  (... (file-name f) ...
       (file-size f) ...))


(define-struct dir [name dirs files])

; A Directory is a (make-dir String [List-of Directory] [List-of File])
; - name is the name of the directory
; - dirs is the list of sub-directories in this directory
; - files is the list of files in this directory
;   (not including the ones in sub-directories)
 
(define DIR-EMPTY (make-dir "nada" '() '()))

(define DIR-PERSONAL (make-dir "personal"
                               (list DIR-EMPTY)
                               (list FILE-CV FILE-PIC)))

(define DIR-CS2500 (make-dir "fundies" '() (list FILE-HELLO)))
(define DIR-CS3200 (make-dir "db" '() (list FILE-P1 FILE-P2)))

(define DIR-SCHOOL (make-dir "school"
                             (list DIR-CS2500 DIR-CS3200)
                             (list FILE-SCHED)))

(define DIR-ALL (make-dir "stuff" (list DIR-PERSONAL DIR-SCHOOL) '()))

(define (dir-temp d)
  (... (dir-name d) ...
       (lod-temp (dir-dirs d)) ...
       (lof-temp (dir-files d)) ...))

; TODO #1: design the function bytes-on-disk that computes the total size of
; a supplied directory (in bytes). You may assume that directories take up 1
; byte for each character in their name. The size of a file is represented
; in the structure (and there is no additional space required for the name).
; For example, the supplied DIR-EMPTY example requires 4 bytes of space,
; while DIR-ALL requires 1,723,533 bytes; you MUST create tests for these
; examples - you can create additional tests but are not required to do so.

; IMPORTANT: you are REQUIRED to use list abstraction(s) where appropriate
;            for this function.

;; bytes-on-disk : Directory -> Nat
;; Computes the total size of a supplied directory (in bytes).

(check-expect
 (bytes-on-disk DIR-EMPTY) 4)

(check-expect
 (bytes-on-disk DIR-PERSONAL) 1434012)

(check-expect
 (bytes-on-disk DIR-ALL) 1723533)

(define (bytes-on-disk dir)
  (foldr + (string-length (dir-name dir))
         (append (map bytes-on-disk (dir-dirs dir))
                 (map file-size (dir-files dir)))))

; TODO #2: design the function keep-file-if that takes a directory and a predicate
; and returns a new directory containing only those files that satisfy the predicate
; (as well as all the original subdirectories). For example, if we keep only those
; files that are smaller than 1000000 bytes, DIR-ALL would remain the same; however,
; if we kept only those files that contain the string ".rkt", then FILE-HELLO would
; be the only remaining file. For clarity, these have been included as example tests;
; you can create additional tests but are not required to do so.

; IMPORTANT: You are NOT allowed to use list abstractions for this function!
;            Instead, you are REQUIRED to follow the templates related to
;            the function inputs - look back to Lab 10 for those templates :)



(check-expect
 (keep-file-if
  DIR-ALL
  (λ (f) (< (file-size f) 1000000)))
 DIR-ALL)

(check-expect
 (keep-file-if
  DIR-ALL
  (λ (f) (string-contains? ".rkt" (file-name f))))
 (make-dir
  "stuff"
  (list (make-dir "personal"
                  (list DIR-EMPTY)
                  '())
        (make-dir "school"
                  (list DIR-CS2500
                        (make-dir "db" '() '()))
                  '()))
  '()))

;; keep-file-if : Directory [File -> Boolean] -> Directory
;; Returns a new directory containing only those files that satisfy the predicate
;; (as well as all the original subdirectories).

(define (keep-file-if d p?)
  (local [; keep-file-if/d : Directory -> Directory
          ; Filters a directory.
          (define (keep-file-if/d d)
            (make-dir (dir-name d) 
                      (keep-file-if/lod (dir-dirs d))
                      (keep-file-if/lof (dir-files d))))

          ; keep-file-if/lod : [Listof Directory] -> [Listof Directory]
          ;; Filters a list of directories.
          (define (keep-file-if/lod lod)
            (cond
              [(empty? lod) '()]
              [(cons? lod)
               (cons
                (keep-file-if/d (first lod))
                (keep-file-if/lod (rest lod)))]))

          ; keep-file-if/lof : [Listof File] -> [Listof File]
          ;; Filters a list of files.
          (define (keep-file-if/lof lof)
            (cond
              [(empty? lof) '()]
              [(cons? lof)
               (if (p? (first lof))
                   (cons (first lof)
                         (keep-file-if/lof (rest lof)))
                   (keep-file-if/lof (rest lof)))]))]
    (keep-file-if/d d)))


          