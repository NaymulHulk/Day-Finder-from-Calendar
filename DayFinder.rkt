;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname DayFinder) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))


; http://mathforum.org/dr.math/faq/faq.calendar.html
; algorithm referred from this website (Zeller's Rule).
; f = k + [(13*m-1)/5] + D + [D/4] + [C/4] - 2*C



(define (k number)
  (modulo number 100))

(define (org_m number)
  (/(-(modulo number 10000)
      (k number))
    100))

(define (m number)
  (cond
    [(= (org_m number) 1) 11]
    [(= (org_m number) 2) 12]
    [else (- (org_m number) 2)]))

(define (org_D number)
  (/(-(modulo number 1000000)
      (modulo number 10000))
    10000))

(define (D number)
  (cond
    [(or (= (m number) 11) (= (m number) 12))
     (- (org_D number) 1)]
    [else (org_D number)]))

(define (C number)
  (/(- number
       (modulo number 1000000))
    1000000))

(define (f number)
  (-(+
     (k number)
     (floor
      (/ (- (* 13 (m number))
            1) 5)) 
     (D number)
     (floor (/ (D number)
               4))
     (floor (/ (C number)
               4)))
    (* 2 (C number))))
   
(define (day number)
  (modulo (f number)
          7))

; (date->day-of-week number) is a function which produces
; a day after the date is input as a number of 8 digits
; Contract:
; date->day-of-week number: Nat -> Sym
; Example:
(check-expect (date->day-of-week 38781202) 'Monday)
(check-expect (date->day-of-week 1920303) 'Invalid)

  
(define(date->day-of-week number)
  (cond
    [(< number 17521001) 'Invalid]
    [(or(< (org_m number) 1) (> (org_m number) 12)) 'Invalid]
    [(or(< (k number) 1) (> (k number) 31)) 'Invalid]
    [(and (= (m number) 12) (> (k number) 28)) 'Invalid] 
    [(= 0 (day number)) 'Sunday]
    [(= 1 (day number)) 'Monday]
    [(= 2 (day number)) 'Tuesday]
    [(= 3 (day number)) 'Wednesday]
    [(= 4 (day number)) 'Thursday]
    [(= 5 (day number)) 'Friday]
    [(= 6 (day number)) 'Saturday]
    [else 'Invalid]))



; Tests:
(check-expect (date->day-of-week 18321401) 'Invalid)
(check-expect (date->day-of-week 00000000) 'Invalid)
(check-expect (date->day-of-week 1000000000)'Invalid)
(check-expect (date->day-of-week 20180123) 'Tuesday)
(check-expect (date->day-of-week 20180513) 'Sunday)
(check-expect (date->day-of-week 69696969) 'Invalid)
(check-expect (date->day-of-week 99991231) 'Friday)
(check-expect (date->day-of-week 20180229) 'Invalid)
(check-expect (date->day-of-week 20180228) 'Wednesday)






    




