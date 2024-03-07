;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; In most organizations, employeers are arranged in a hierarchy called an
;; "organization chart" or org-chart. For example, here is a portion of the 
;; org-chart for Northeastern:
;;
;;                      Joseph E. Aoun
;;                      (President)
;;                           |
;;                      The Cabinet                                   
;;                           |
;;       +-------------------+-------------------+
;;   Karl Reid      Madeleine Estabrook    David Madigan
;; (Chief Inclusion  (Student Affairs)    (Academic Affairs)
;;     Officer)                                 |
;;                    +-------------------------+---------+
;;                    |                                   |
;;              Administration                      Academic Deans
;;                    |                                   |
;;                    |                  +----------------+---------------+
;;                    |                  |                |               | 
;;              Thomas Sheahan      Alan Mislove    Carmen Sceppa    Uta Poiger
;;         (Curriculum & Programs)    (Khoury)         (Bouve)     (Social Sciences &
;;                                                                      Humanities)
;;
;; The people in this chart have a name (obviously) and a title. Each also
;; has a number of "direct reports" (possible no direct reports). In addition,
;; each set of direct reports is grouped together with a label. E.g., the three
;; groups above are "The Cabinet", "Administration", and "Academic Affairs".
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part A
;;
;; Design data called OrgChart to represent an org-chart.
;; One of your examples must have *all* the information in the org-chart picture
;; shown above.

;; [TODO] Data design

(define-struct group [label people])

;; A Group is a (make-group String [Listof OrgChart])
;; - label is the name of the grouping of the direct reports
;; - people is a list of direct reports the group has, either another group or a person
;; Interpretation: represents a set of "direct reports" grouped together with a label
;; containing either another group or a person.

(define (group-temp g)
  (... (group-label g) ...
       (orgchart-temp (group-people g)) ...))

(define-struct person [name title dr])

;; A Person is a (make-person String String [Listof OrgChart])
;; - name is the name of the person
;; - title is the position the person holds in the organization
;; - dr is a list of direct reports the person has, either a group or person
;; Interpretation: represents a person in the organization with their
;; respective title and direct reports, either another person or a group.

(define (person-temp p)
  (... (person-name p) ...
       (person-title p) ...
       (orgchart-temp (person-dr p)) ...)) 

(define P-POIGER (make-person "Uta Poiger" "Social Sciences & Humanities" '()))

(define P-SCEPPA (make-person "Carmen Sceppa" "Bouve" '()))

(define P-MISLOVE (make-person "Alan Mislove" "Khoury" '()))

(define P-SHEAHAN (make-person "Thomas Sheahan" "Curriculum & Programs" '()))

(define GP-DEANS
  (make-group "Academic Deans" (list P-MISLOVE P-SCEPPA P-POIGER)))

(define GP-ADMIN
  (make-group "Administration" (list P-SHEAHAN)))

(define P-MADIGAN (make-person "David Madigan" "Academic Affairs" (list GP-ADMIN GP-DEANS)))

(define P-ESTABROOK (make-person "Madeleine Estabrook" "Student Affairs" '()))

(define P-REID (make-person "Karl Reid" "Chief Inclusion Officer" '()))

(define GP-CABINET
  (make-group "The Cabinet" (list P-REID P-ESTABROOK P-MADIGAN)))

(define P-AOUN
  (make-person "Joseph E. Aoun" "President" (list GP-CABINET)))


;; An OrgChart is one of:
;; (make-group String [Listof Orgchart])
;; (make-person String String [Listof OrgChart])
;; Interpretation: represents an organization chart

(define OC-SCEPPA P-SCEPPA)

(define OC-DEANS GP-DEANS)

(define OC-AOUN P-AOUN)

(define (orgchart-temp oc)
  (...
   (cond
     [(group? oc) ...
      (group-label oc) ...
      (orgchart-temp (group-people oc)) ...]
     [(person? oc) ...
      (person-name oc) ...
      (person-title oc) ...
      (org-chart-temp (person-dr oc)) ...])))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part B

;; Design the function num-peeps that counts how many people are in an OrgChart.

;; [TODO] Function design

;; num-peeps : OrgChart -> Nat
;; Counts how many people are in an OrgChart.

(check-expect
 (num-peeps OC-SCEPPA)
 1)

(check-expect
 (num-peeps OC-DEANS)
 3)

(check-expect
 (num-peeps OC-AOUN)
 8)

(define (num-peeps oc)
  (cond
    [(group? oc) 
     (count-peeps (group-people oc))]
    [(person? oc) 
     (+ 1
        (count-peeps (person-dr oc)))]))
  
;; count-peeps : [Listof OrgChart] -> Nat
;; Counts the number of people in a list of OrgChart.

(check-expect
 (count-peeps (list OC-SCEPPA))
 1)

(check-expect
 (count-peeps (list OC-AOUN))
 8)

(define (count-peeps loc)
  (foldr + 0 (map num-peeps loc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part C

;; Design the function full-title that consumes an OrgChart and the name of
;; an organization, and produces an OrgChart that adds the "(organization name)"
;; to the title for each person in the OrgChart.

;; full-title : OrgChart String -> OrgChart
;; Produces an OrgChart that adds the "(organization name)"
;; to the tile for each person in the Orgchart.

(check-expect
 (full-title OC-SCEPPA "Northeastern")
 (make-person
  "Carmen Sceppa"
  "Bouve (Northeastern)"
  '()))

(check-expect
 (full-title OC-DEANS "Northeastern")
 (make-group
  "Academic Deans"
  (list
   (make-person
    "Alan Mislove"
    "Khoury (Northeastern)"
    '())
   (make-person
    "Carmen Sceppa"
    "Bouve (Northeastern)"
    '())
   (make-person
    "Uta Poiger"
    "Social Sciences & Humanities (Northeastern)" '()))))

(check-expect
 (full-title OC-AOUN "Northeastern")
 (make-person
  "Joseph E. Aoun"
  "President (Northeastern)"
  (list (make-group
         "The Cabinet"
         (list (make-person
                "Karl Reid"
                "Chief Inclusion Officer (Northeastern)"
                '())
               (make-person
                "Madeleine Estabrook"
                "Student Affairs (Northeastern)"
                '())
               (make-person
                "David Madigan"
                "Academic Affairs (Northeastern)"
                (list (make-group
                       "Administration"
                       (list (make-person
                              "Thomas Sheahan"
                              "Curriculum & Programs (Northeastern)"
                              '())))
                      (make-group
                       "Academic Deans"
                       (list (make-person
                              "Alan Mislove"
                              "Khoury (Northeastern)"
                              '())
                             (make-person
                              "Carmen Sceppa"
                              "Bouve (Northeastern)"
                              '())
                             (make-person
                              "Uta Poiger"
                              "Social Sciences & Humanities (Northeastern)"
                              '()))))))))))

(define (full-title oc org)
  (local [; rename-title : Person -> Person
          ; Adds the organization name to the title of the person
          (define (rename-title p)
            (make-person (person-name p)
                         (string-append (person-title p) " (" org ")")
                         (map (λ (oc2) (full-title oc2 org)) (person-dr p))))]
    (cond
      [(group? oc)
       (make-group
        (group-label oc)
        (map rename-title (group-people oc)))]
      [(person? oc)
       (rename-title oc)])))


