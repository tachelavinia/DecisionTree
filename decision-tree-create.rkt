#lang racket

(require test-engine/racket-tests)
(include "decision-tree-test.rkt")

;;
;;             Tema 1  - Paradigme de programare
;;                Racket: Constructia arborilor de decizie
;;               Implementare: Lavinia Tache
;;                            322CA  
;;


;; pentru frunze:

;; primește un nod; întoarce #t dacă acesta este frunză, #f altfel
(define is-leaf?
  (lambda (node)
   (not (pair? node))))

;; primește un nod frunză; întoarce clasa exemplelor din frunză
(define get-leaf-class
  (lambda (node)
    node))

;; pentru frunze speciale (BONUS):

;; primște un nod; întoarce #t dacă nodul este frunză specială (frunză fără exemple sau cu exemple de clase diferite), #f altfel
;; dacă nu implementați bonus, trebuie ca funcția să întoarcă #f
(define is-special-leaf?
  ; TODO
  (λ (node) #f)
  )

;; primște un nod frunză specială; întoarce tipul frunzei speciale (trebuie să fie unul dintre membrii 2 și 3 ai strings
;; clasa exemplelor din frunza specială va fi verificată tot cu get-leaf-class
(define get-special-leaf-type
  ; TODO
  (λ (node) #f)
  )


;; pentru noduri care nu sunt frunze:

;; primște un nod; întoarce numele atributului verificat în nod
(define get-attribute-name
  (lambda (node) 
    (car node)))

;; primește un nod și o valoare a atributului verificat în nod
;; întoarce nodul copil (nod intern sau frunză) corespunzător valorii date

;intoarce toti copii unui nod iar functia get-child 
(define kids
    (lambda (node)
      (filter 
        (lambda (elem)
            (pair? elem)) node)))

(define (get-child-helper node value)
  (if (null? node)
          null
          (if (equal? (caar node) value)
              (cdar node)
              (get-child-helper (cdr node) value))))

;returneaza copiii pentru care atributul este value
(define get-child 
  (lambda (node value)
     (get-child-helper (kids node) value)))


;; opțional: verificare nod
;; primește un argument; întoarce #t dacă argumentul este o reprezentare corectă a unui nod (frunză sau nu) din arbore; #f altfel
(define is-node?
  (lambda (node)
    (if (list? node)
         #t
         node)))

; asamblare funcții de acces arbore
(define functions (list is-leaf? get-leaf-class is-special-leaf? get-special-leaf-type get-attribute-name get-child is-node?))


;; TASK (pregătitor):
;; scrieți (manual) în formatul ales un arbore de decizie pentru exemple conținând 1 atribut - shape, care are două valori - round și square
;; un exemplu va fi în clasa "yes" dacă este rotund, și în "no" altfel
;; arborele trebuie să fie astfel:

;;    shape
;;     / \
;; round square
;;   /     \
;; yes     no


(define tree-1 
  '(shape (round . yes) (square . no)))

(check-expect (is-node? tree-1) #t)
(check-expect (is-leaf? tree-1) #f)
(check-expect (get-attribute-name tree-1) 'shape)
(check-expect (not (get-child tree-1 'round)) #f)
(check-expect (not (get-child tree-1 'square)) #f)
(check-expect (is-leaf? (get-child tree-1 'round)) #t)
(check-expect (is-leaf? (get-child tree-1 'square)) #t)
(check-expect (is-special-leaf? (get-child tree-1 'round)) #f)
(check-expect (get-leaf-class (get-child tree-1 'round)) 'yes)
(check-expect (get-leaf-class (get-child tree-1 'square)) 'no)


;; TASK
;; scrieți funcția de mai jos pentru a calcula entropia unui set de exemple, fiecare exemplu conținând informație despre clasa sa
;; funcția log2 este implementată în decision-tree-test

;; examples: o listă de exemple (setul S), nevidă, în care fiecare exemplu este o listă de perechi, una dintre ele fiind (<nume-atribut-clasă> <clasă>)
;; class-attribute: o listă de forma (<nume-atribut-clasă> <clasă-1> <clasă-2> <clasă-3>)
;; întoarce: entropia setului de exemple în raport cu clasa, H(S) = - sumă-peste-clase p(clasă)*log2 p(clasă)
;;   unde p(clasă) este numărul de exemple cu clasa <clasă> împărțit la numărul de exemple din setul S


;;calculez numarul de aparitii ale unei clase in lista in exemple
(define (classOccurrence-helper examples class)
  (map
   (lambda (lista)
     (filter
      (lambda (elem)
        (equal? class (cdr elem))) lista)) examples))


(define (checkNullElements lista counter)
  (if (null? lista)
      counter
      (if (null? (car lista))
          (checkNullElements (cdr lista) (+ 0 counter))
          (checkNullElements (cdr lista) (+ 1 counter)))))

(define (classOccurrence examples class)
  (checkNullElements (classOccurrence-helper examples class) 0))

;;pentru calculul entrpoiei calculez procentul : numarul de aparitii ale unui clase in lista de exemple
;; raportata la dimensiunea clasei
(define (getProcentage examples value)
  ( / (classOccurrence examples value) (length examples)))

;; termenii din suma pentru calculul entropiei sunt reprezentati de produsul dintre procentul clasei si logaritm in baza 2 din aceasta valoare
(define (terms examples value)
  (if (zero? (getProcentage examples value))
      0
      ( * (getProcentage examples value) (log2 (getProcentage examples value)))))

(define (compute-enthropy-helper examples classList)
  (if (null? examples)
      0
      (if (null? classList)
          0
          ( + (terms examples (car classList)) (compute-enthropy-helper examples (cdr classList))))))

(define (compute-enthropy examples classList)
  (- 0 (compute-enthropy-helper examples classList)))

(define tolerance 0.001)
(check-within (compute-enthropy '() '(classname yes no)) 0 tolerance) ; expect error
(check-within (compute-enthropy '(((shape . round) (classname . yes)) ((shape . square) (classname . yes))) '(classname yes no)) 0 tolerance)
(check-within (compute-enthropy '(((shape . round) (classname . no)) ((shape . square) (classname . no))) '(classname yes no)) 0 tolerance)
(check-within (compute-enthropy '(((shape . round) (classname . yes)) ((shape . square) (classname . no))) '(classname yes no)) 1 tolerance)
(check-within (compute-enthropy '(((shape . round) (classname . yes)) ((shape . square) (classname . no)) ((shape . square) (classname . no))) '(classname yes no maybe)) 0.918 tolerance)
(check-within (compute-enthropy '(((shape . round) (classname . yes)) ((shape . square) (classname . no)) ((shape . square) (classname . maybe))) '(classname yes no maybe)) 1.584 tolerance)

;; TASK
;; scrieți funcția de mai jos pentru a calcula câștigul informațional al unui atribut în raport cu clasa, pentru un set de exemple

;; examples: o listă de exemple, nevidă, în care fiecare exemplu este o listă de perechi, una dintre ele fiind (<nume-atribut-clasă> <clasă>)
;; attribute: o listă de forma (<nume-atribut> <valore-1> <valoare-2> <valoare-3>)
;; class-attribute: o listă de forma (<nume-atribut-clasă> <clasă-1> <clasă-2> <clasă-3>)
;; întoarce: câștigul informațional al atributului, G(S, A) = H(S) - sumă-peste-valori-A p(v)*H(Sv)
;;   unde p(v) este numărul de exemple cu valoarea v pentru A împărțit la numărul de exemple din S
;;   iar Sv este mulțimea exemplelor din S care au valoarea v pentru A
;

;; castigul informational reprezinta diferenta dintre entropie si suma dintre entropia calculata pe fiecare atribut
(define Hs
  (lambda (examples class-attribute)
    (compute-enthropy examples class-attribute)))

;;calculez astfel lista formata doar din atributul cautat -> Sv , verificand mai intai daca acesta exista in lista de exemple
(define (getList examples value)
  (filter
   (lambda (list)
     (equal? value (cdar list))) examples))

(define (checkExistance list value)
  (if (null? list)
      #f
      (if (equal? (cdar list) value)
          #t 
          (checkExistance (cdr list) value))))

(define (attributeList examples value)
  (filter 
   (lambda (list)
     (checkExistance list value)) examples))

(define (attributeList-helper examples value)
  (map
   (lambda (list)
     (pair? (assoc value list))) examples))

;;procentul este de data aceasta diferit intrucat se calculeaza prin raportul dintre numarul de aparitii ale valorii in lista formata
;; din clasa respectiva ( generata anterior ) si lungimea totala a listei de exemple
(define (pvClasa examples list value)
  (if (null? examples)
      0
      ( / (classOccurrence examples value) (length examples))))

(define Ha
  (lambda (examples attribute classname)
    (if ( null? attribute)
        0
        ( + ( * (pvClasa examples (attributeList examples (car attribute)) (car attribute)) (compute-enthropy (attributeList examples (car attribute)) classname))
            (Ha examples (cdr attribute) classname)))))

(define compute-gain
  (lambda (examples attribute class-attribute)
    ( - (Hs examples class-attribute) (Ha examples (cdr attribute) class-attribute))))

(check-within (compute-gain 
               '(((shape . round) (classname . yes)) ((shape . square) (classname . yes)))
               '(shape round square)
               '(classname yes no)
               ) 0 tolerance)
(check-within (compute-gain 
               '(((shape . round) (classname . no)) ((shape . square) (classname . yes)))
               '(shape round square)
               '(classname yes no)
               ) 1 tolerance)
(check-within (compute-gain 
               '(((shape . round) (classname . no)) ((shape . round) (classname . yes)))
               '(shape round square)
               '(classname yes no)
               ) 0 tolerance)
(check-within (compute-gain 
               '(((shape . round) (size . 1) (classname . yes))
                 ((shape . round) (size . 2) (classname . no))
                 ((shape . square) (size . 1) (classname . yes))
                 ((shape . square) (size . 2) (classname . yes)))
               '(shape round square)
               '(classname yes no)
               ) 0.311 tolerance)
(check-within (compute-gain 
               '(((shape . round) (size . 1) (classname . yes))
                 ((shape . round) (size . 2) (classname . no))
                 ((shape . square) (size . 1) (classname . yes))
                 ((shape . square) (size . 2) (classname . no))
                 ((shape . square) (size . 2) (classname . yes)))
               '(size 1 2)
               '(classname yes no)
               ) 0.419 tolerance)



;; TASK
;; creați un arbore de decizie pentru mulțimea dată de exemple, pentru muțimea dată de atribute, și pentru clasa dată

;; examples: o listă de exemple, nevidă, în care fiecare exemplu este o listă de perechi, una dintre ele fiind (<nume-atribut-clasă> <clasă>)
;; attributes: o listă de liste de forma (<nume-atribut> <valore-1> <valoare-2> <valoare-3>)
;; class-attribute: o listă de forma (<nume-atribut-clasă> <clasă-1> <clasă-2> <clasă-3>)
;; întoarce: un arbore de decizie, în formatul ales, corespunzător cu argumentele date

;; functia returneaza valoarea primei clase din lista de exemple
;; Acest fapt este atunci la conditia de oprire din crearea arborelui : daca toate frunzele au aceeasi clase returneaza un nou cu clasa respectiva
(define (getFirstClassValue examples class-attributes)
  (if (null? examples)
      null
      (cdr (assoc (car class-attributes) (car examples)))))

;;returneaza o lista care va contine doar classname-urile din lista de exemple
(define (checkUniqueClass examples classes)
  (map
   (lambda (list)
     (assoc (car classes) list)) examples))

;; verifica daca toate nodurile au aceeasi clasa
(define (checkIfDifferent list)
  (null? (filter
          (lambda (pair)
            (not (equal? (cdar list) (cdr pair)))) (cdr list))))

;;sterge un element din lista
(define (deleteListFromList list value)
  (filter
   (lambda (elem)
     (not (equal? value (car elem)))) list))

;;calculeaza castigul informational pentru lista de atribute astfel construita
(define (computeGain examples attribute classname)
  (map
   (lambda (list)
     (cons (car list) (compute-gain examples list classname))) attribute))

;; pentru a obtine un arbore cat mai optim calculez valoarea maxima a castigului, iar in constructia arborelui se va alege nodul cu
;; castig informational maxim
(define (maxGain list)
  (if (null? list)
      0
      (if ( > (cdar list) (maxGain (cdr list)))
          (cdar list)
          (maxGain (cdr list)))))

;; valorile castigului informational sunt stocate intr-o lista sub forma de prechi (atribut, valoare_castig) ca in exemplul :
;; '((Food . 0.018796874098554683) (Chat . 0.0) (Speedy . 0.0) (Price . 0.018796874098554683) (Bar . 0.0))
(define (attMax list value)
  (car (filter
        (lambda (pair)
          (equal? (cdr pair) value)) list)))

;;nodul va fi reprezentat de atributul cu valorea maxima , iar de el voi lega valorile atributul.
;;Functia returneaza valorile pentru un atribut dat
(define (getValuesForAttribute attributes value)
  (cdar (filter
         (lambda (list)
           (equal? (car list) value)) attributes)))

(define (myAttributeList attributes value)
  (cdar (filter 
   (lambda (list)
      (equal? value (car list))) attributes)))

;; Functia create-tree urmareste pasii algoritmului de constructie:
;;   daca exemplele sunt in aceeasi clasa va intoarce o frunza cu clasa respectiva
;;  in caz contrar se alege din multimea de atribute atributul cu castig informational maxim
;;  Scot ulterior atributul din lista de atribute si continui functia de creare arbore recursiv
;;  pe valorile atributului. Se creeaza un nod nou, avand dreot copii nodurile corespunzatoare valorilor pentru atributul A
(define create-tree
  (lambda (examples attributes class-attribute)
    (if ( null? examples)
        null
        (if (checkIfDifferent (checkUniqueClass examples class-attribute))
           (getFirstClassValue examples class-attribute)
            (let* ( [gainList  (computeGain examples attributes class-attribute)]
                    [maxValue (maxGain gainList)]
                    [nodeAttr (car (attMax gainList maxValue))])
              (cons nodeAttr 
                  (map 
                     (lambda (value)
                       (cons value (create-tree (attributeList examples value) (deleteListFromList attributes nodeAttr) class-attribute)))
                            (myAttributeList attributes nodeAttr))))))))
  
  
  (define I-DID-THE-BONUS #f)
  
  (check-expect (perform-test functions 'food-small create-tree) #t)
 ;(check-expect (perform-test functions 'food-big create-tree) #t)
  (check-expect (perform-test functions 'objects create-tree) #t)
  (check-expect (perform-test functions 'weather create-tree) #t)
  
  (check-expect (and (perform-test functions 'food-small create-tree) (get-tree-height functions (get-tree 'food-small create-tree) (get-test 'food-small))) 2)
  ;(check-expect (and (perform-test functions 'food-big create-tree)   (get-tree-height functions (get-tree 'food-big create-tree) (get-test 'food-big)))   4)
  (check-expect (and (perform-test functions 'weather create-tree)    (get-tree-height functions (get-tree 'weather create-tree) (get-test 'weather)))    3)
  (check-expect (and (perform-test functions 'objects create-tree)    (get-tree-height functions (get-tree 'objects create-tree) (get-test 'objects)))    3)
  
  ;(if I-DID-THE-BONUS (display "BONUS DONE\n") (display "bonus not done\n"))
  ;       (check-expect (if I-DID-THE-BONUS (perform-test functions 'bonus create-tree) #t) #t)
  ;(when I-DID-THE-BONUS (display (get-tree 'bonus create-tree)) (newline))

  
(test)
