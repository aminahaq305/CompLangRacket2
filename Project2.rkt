#lang racket
;Amina Haq | Project 2 | Recursive Functions in Racket

;Check if two lists are equal
;-----------------------------
(define set-equal?
  (lambda (set1 set2)
    (letrec((members-equal
              (lambda (set1 set2)
                (or (null? set1) ;base case: set1 is null
                    (and (member (car set1) set2) ;the first element of set1 is in set2
                         (members-equal (cdr set1) set2)))))) ;check if each of he rest of the elements of set1 are in set2
      (and (members-equal set1 set2) ;lists are equal if the members of set1 are equal to members of set 2, AND
           (members-equal set2 set1))))) ;members of set2 are equal to members of set1

;Union two lists together
;-------------------------
(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2) ;if set1 is empty, return set2
          ((member (car set1) set2) ;if the first element of set1 is in set2 then
           (union (cdr set1) set2)) ;call union on rest of set1 and set2
          (else (cons (car set1) ;create a list and add the first element of set1, then call union on rest of set1 and set2
                      (union (cdr set1) set2))))))

;Find intersection of two lists
;-------------------------------
(define intersection
  (lambda (set1 set2)
    (cond ((null? set1) '()) ;base case: set1 is null return empty list
          ((member (car set1) set2) ;if first element of set1 is in set2
           (cons (car set1) ;then create list and put first element of set1 and 
                 (intersection (cdr set1) set2))) ;call intersection function recursively on rest of set1
          (else (intersection (cdr set1) set2))))) ;otherwise simply call intersection function recursively on rest of set1

;Mergesort function
;-----------------
(define mergesort (lambda (lst)
                    (letrec ((merge (lambda (lst lst1)
                                      (if (null? lst) lst1
                                          (if (null? lst1) lst
                                              (if (< (car lst) (car lst1))
                                                  (cons (car lst) (merge (cdr lst) lst1))
                                                  (cons (car lst1) (merge (cdr lst1) lst))))))))
                      (letrec ((split (lambda (lst)
                                        (letrec ((odd (lambda (lst)
                                                        (if (null? lst) '()
                                                            (if (null? (cdr lst)) (list (car lst))
                                                                (cons (car lst) (odd (cddr lst))))))))
                                          (letrec ((even(lambda (L)
                                                          (if (null? L) '()
                                                              (if (null? (cdr L)) '()
                                                                  (cons (cadr L) (even (cddr L))))))))
                                            (cons (odd lst) (cons (even lst) `())))))))                 
                        (if (null? lst) lst
                            (if (null? (cdr lst)) lst
                                (merge
                                 (mergesort (car (split lst)))
                                 (mergesort (cadr (split lst))))))))))
;Powerset function
;-----------------
(define powerset
  (lambda (lst)
    (if (null? lst) '(()) ;if null, return null
        (append-map (lambda (x)
                      (list x (cons (car lst) x))) ;map then append to list x
                    (powerset (cdr lst))))))

;Nested Set Equal
;----------------
(define nested-set-equal? (lambda (list1 list2)
                       (letrec(( checklist (lambda (list1 sublist)
                                             (if (list? (car list1))
                                                 (append (car list1) sublist)
                                                 (checklist (cdr list1) sublist)))))
                         (and (set-equal?
                               (checklist list1 '())
                               (checklist list2 '()))
                              (set-equal?
                               (remove (checklist list1 '()) list1)
                               (remove (checklist list2 '()) list2))))))

;Nested Set Reduce
;-----------------
(define nested-reduce (lambda (list1)
                        (letrec((removeDup (lambda (x emp)
                                             (cond ((empty? x) emp)
                                                   ((not (member (car x) emp)) (removeDup (cdr x) (append emp
                                                                                                          (list(car x)))))
                                                   (else (removeDup (cdr x) emp))))))
                          
                          (letrec ((nestedcheck (lambda (y)
                                                  (cond ((empty? y) '())
                                                        (else (
                                                               if (list? (car y))
                                                                  (cons(removeDup (car y) '()) (nestedcheck (cdr y)))
                                                                  (cons(car y) (nestedcheck (cdr y)))))))))
                            (nestedcheck (removeDup list1 '() )))))) ;remove duplicates in main list, then in nested sublists

;(set-equal? '(1 2 3 4) '(4 2 1 3))
;(nested-set-equal? '(1 2 (3 4 5)) '(4 (2 3 5) 1))
;(union '(1 2 3 4) '(2 3 4 5))
;(intersection '(1 2 3 4) '(2 3 4 5))
;(mergesort '(3 1 2 7 9))
;(powerset '(1 3 5))
;(nested-reduce '(7 (2 5) (2 5) ((2 5) (2 5) 2 2 5) 3 7 1))