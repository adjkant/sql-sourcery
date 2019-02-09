#lang racket
(require "../main.rkt")

(define PROD_DB "partner-pairings.db")
(define TEST_DB "test.db")

(sourcery-db PROD_DB)

;; Define testing actions
(module+ test
  (declare-test-vars students pairings)
  
  (define-action clear-all
    (sourcery-filter-delete (λ (x) #f) (sourcery-load pairing))
    (sourcery-filter-delete (λ (x) #f) (sourcery-load student)))

  (define-action create-students
    (set-test-var! students (list (student-create "Test Student 1" 1   1   #t)
                                  (student-create "Test Student 2" 2   2   #f)
                                  (student-create "Test Student 3" 3   0   #t)
                                  (student-create "Test Student 4" 5   0   #f)
                                  (student-create "Test Student 5" 100 100 #t))))

  (define-action pair-created-students
    (set-test-var! pairings (equal-pair-students students))))


;; A Pair is a (make-pairing String String) where the two strings are the two names
;; of students
(sourcery-struct pairing [(name1 STRING) (name2 STRING)])

;; A Student is a (make-student String Integer Integer Boolean) with a name,
;; homework grade, exam grade, and a boolean telling if they are nice
(sourcery-struct student [(name STRING)
                          (hw-grade INTEGER)
                          (exam-grade INTEGER)
                          (nice? BOOLEAN)])

;; Student Student -> Pair
;; create a pairing of two students
(define (pair-students s1 s2)
  (pairing-create (student-name s1) (student-name s2)))

(module+ test
  (sourcery-test-suite
   "Pairing Two Students"
   #:before (action (pair-students (student-create "Test Student 1" 1 1 #f)
                                   (student-create "Test Student 2" 1 1 #f)))
   #:after clear-all
   (check-equal? (length (sourcery-load pairing)) 1)
   (check-equal? (pairing-name1 (first (sourcery-load pairing))) "Test Student 1")))

;; [List-of Student] -> [List-of Pair]
;; pair a list of students as ordered in the list
(define (pair-in-order los)
  (cond [(or (empty? los) (empty? (rest los))) '()]
        [else (cons (pair-students (first los) (second los))
                    (pair-in-order (rest (rest los))))]))


;; [List-of Student] -> [List-of Pair]
;; pair all students based on matching average hw and exam grades, leaving the
;; best student unpaired
(define (equal-pair-students los)
  (pair-in-order
   (sort los
         (λ (s1 s2) (< (+ (student-hw-grade s1) (student-exam-grade s1))
                       (+ (student-hw-grade s2) (student-exam-grade s2)))))))

(module+ test
  (sourcery-test-suite
   "Equal Pairing Students"
   #:before (action-compose create-students pair-created-students)
   #:after clear-all
   (check-equal? (length pairings) 2)
   (check-equal? (pairing-name1 (first pairings))  "Test Student 1")
   (check-equal? (pairing-name2 (first pairings))  "Test Student 3")
   (check-equal? (pairing-name1 (second pairings)) "Test Student 2")
   (check-equal? (pairing-name2 (second pairings)) "Test Student 4")))

;; Run all tests
(module+ test (run-sourcery-tests TEST_DB PROD_DB))
