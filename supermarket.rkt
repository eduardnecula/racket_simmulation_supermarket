;@NECULA EDUARD-IONUT 322 CA - 2021 - etapa 4
#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)

; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (cond
    [(null? index) counters]
    [(null? counters) counters]
    [(< index 0) counters]
    [else
     (map
      (λ (C)
        (if (= (counter-index C) index)
            (apply f (list C))
            C))
      counters)]))

;functia este similara cu update, doar ca aplica o functie pe fiecare element din lista counters
(define (update-all f counters)
  (cond
    [(null? counters) counters]
    [else
     (map
      (λ (C)
        (apply f (list C))
        counters))
     ]))

(define (tt+ C minutes)
  (match C
    [(counter index tt et queue) (+ tt minutes)]))

(define (et+ C minutes)
  (match C
    [(counter index tt et queue) (+ et minutes)]))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (struct-copy counter C
                 [tt (tt+ C items)]
                 [et (cond
                       [(= 0
                           (+ (queue-size-r (counter-queue C)) (queue-size-l (counter-queue C)))) (et+ C items)]
                       [else (counter-et C)])]
                 [queue
                  (enqueue (cons name items) (counter-queue C))])))

(define functie-mai-abstracta-careia-ii-veti-da-un-nume-sugestiv
  'your-code-here)
(define (get_min_tt counters)
  (apply min (map
              (λ (C)
                (counter-tt C))
              counters)))

;functie auxiliara pentru a calcula min-tt
(define (get_min_struct_tt counters)
  (car
   (filter
    (λ (C)
      (= (get_min_tt counters) (counter-tt C)))
    counters)))

;functia intoarce min-tt dintr-o lista de case
(define (min-tt counters)
  (cons (counter-index (get_min_struct_tt counters)) (counter-tt (get_min_struct_tt counters))))

;functie auxiliare pentru min-et
(define (get_min_et counters)
  (apply min (map
              (λ (C)
                (counter-et C))
              counters)))

;functie auxiliara pentru min-et
(define (get_min_struct counters)
  (car
   (filter
    (λ (C)
      (= (get_min_et counters) (counter-et C)))
    counters)))

;functia intoarce min-et dintr-o lista de case
(define (min-et counters)
  (cond
    [(null? counters) counters]
    [else (cons (counter-index (get_min_struct counters)) (counter-et (get_min_struct counters)))]))

;functie aux pentru a lua toate val dintr-o lista de queue
(define (sum-right L)
  (if (null? L)
      0
      (+ (cdr (car L)) (sum-right (cdr L))
         )
      )
  )
(define (sum-left L)
  (if (stream-empty? L)
      0
      (+ (cdr (stream-first L)) (sum-left (stream-rest L))
         )
      )
  )
;functia aduna toate et-urile din lista-l si din lista-r
(define (get-tt-sum-counters l-left l-right size-l size-r)
  (cond
    [(zero? size-l)
     (sum-right l-right)]
    [(zero? size-r)
     (sum-left l-left)]
    [else (+ (sum-right l-right) (sum-left l-left))]
    ))

(define (get-et-next-person q)
  (cond
    [(and (= (queue-size-l q) 1)  (= 1 (queue-size-r q))) (cdr (car (queue-right q)))]
    [(and
      (and (>= (queue-size-l q) 2) (= (queue-size-l q) (queue-size-r q)))
      (and (>= (queue-size-r q) 2) (= (queue-size-l q) (queue-size-r q))))
     (cdr (stream-first (stream-rest (queue-left q))))
     ]
    [(> (queue-size-l q) (queue-size-r q)) (cdr (stream-first (stream-rest (queue-left q))))]
    [else 
     [(< (queue-size-l q) (queue-size-r q)) (cdr (car (queue-right q)))]
     ]
    )
  )

(define (remove-first-from-counter C)   ; testată de checker
  (cond
    [(queue-empty? (counter-queue C)) C]
    [else
     (if (= 1 (+ (queue-size-r (counter-queue C)) (queue-size-l (counter-queue C))))
         (struct-copy counter C [tt 0] [et 0]
                      [queue (dequeue (counter-queue C))])
         (struct-copy counter C
                      [tt (- (get-tt-sum-counters
                              (queue-left (counter-queue C))
                              (queue-right (counter-queue C))
                              (queue-size-l (counter-queue C))
                              (queue-size-r (counter-queue C)))
                             (cdr (top (counter-queue C))))]
                      [et (get-et-next-person (counter-queue C))]
                      [queue (dequeue (counter-queue C))]))]))

; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (cond
      [(= (and (counter-tt C) (counter-et C)) 0)
       (struct-copy counter C [tt 0] [et 0])
       ]
      [else (struct-copy counter C
                         [tt
                          (if (>= (counter-tt C) minutes)
                              (- (counter-tt C) minutes)
                              0)]
                         [et
                          (if (>= (counter-et C) minutes)
                              (- (counter-et C) minutes)
                              0)])])))


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (calculate-sum-tt List)
  (if (null? List)
      0
      (+ (counter-tt (car List)) (calculate-sum-tt (cdr List)))))

;functie care calculeaza tt mediu
(define (get-mediu-tt List)
  (/ (calculate-sum-tt List) (length List)))

;functia da indexul ultimei liste din casele slow
(define (get-last-index List)
  (if (null? List)
      List
      (counter-index (car (reverse List)))))



;functie care modifica tt-ul unei case cu minutes
(define (modifty-C-tt C minutes)
  (struct-copy counter C
               [tt (tt+ C minutes)]
               [et (et+ C minutes)]))
;functia de baza, apelata de checker
(define (serve requests fast-counters slow-counters)
  (serve-aux requests fast-counters slow-counters null))

;functia care ma ajuta sa returnez functia de baza + lista cu clientii care ies


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei


(define (serve-aux requests fast-counters slow-counters exit-list)
  (if (null? requests)
      (cons exit-list (append
                       (map
                        (λ (c)
                          (cons
                           (counter-index c)
                           (counter-queue c)
                           
                           )
                          )
                       fast-counters
                       )

                       (map
                        (λ (c)
                          (cons
                           (counter-index c)
                           (counter-queue c)
                           )
                          )
                       slow-counters
                       )

                       ))
      (match (car requests)
        ;trec x minutes
        ;trebuie sa actualiez
        ;et si tt -> am functie
        ;si queue ->TODO
        [(list 'ensure time)
         (serve-aux (cdr requests) fast-counters slow-counters exit-list)
         ]
        
        [(list name n-items)
         (if (<= n-items ITEMS)
             ;pentru casele rapide
             ;daca casa fast are min-tt mai mic o aleg pe ea
             ;altfel voi alege casa slown
             (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                 (serve-aux (cdr requests)
                            (update (λ (x)
                                      ;(modify-C  x name n-items)
                                      ((add-to-counter name n-items) x))
                                    fast-counters
                                    (car (min-tt fast-counters)))
                            slow-counters
                            exit-list)
                 (serve-aux (cdr requests)
                            fast-counters
                            ;casele slow le updatez
                            (update (λ (x)
                                      ((add-to-counter name n-items) x)) ;functia care adauga la case tot
                                    slow-counters
                                    (car (min-tt slow-counters)))
                            exit-list
                            ))
             ;pentru casele lente
             (serve-aux (cdr requests)
                        fast-counters
                        ;casele slow le updatez
                        (update (λ (x)
                                  ((add-to-counter name n-items) x))
                                ;functia care adauga la case tot
                                slow-counters
                                (car (min-tt slow-counters)))
                        exit-list))]
        [(list 'close x)
         (serve-aux (cdr requests) fast-counters slow-counters exit-list)
         ]
        [(list 'delay index minutes)
         (serve-aux (cdr requests)
                    (update (λ (x)
                              (modifty-C-tt x minutes))
                            fast-counters
                            index)
                    (update (λ (x)
                              (modifty-C-tt x minutes))
                            slow-counters
                            index)
                    exit-list
                    )]
        [x
         (serve-aux (cdr requests) ;trec la urm request
                    ;fast counter
                    (map 
                     (λ (c)
                       (cond
                         [(zero? (counter-et c)) c]
                         [(> (counter-et c) x) ((pass-time-through-counter x) c)]
                         [(= (counter-et c) x) ((pass-time-through-counter x) (remove-first-from-counter c))]
                         [else
                          (let
                              ((rest (- x (counter-et c))))
                            ((pass-time-through-counter rest) (remove-first-from-counter c))
                            )
                          ]
                         )
                       )
                     fast-counters)
                    ;slow counters
                    (map 
                     (λ (c)
                       (cond
                         [(zero? (counter-et c)) c]
                         [(> (counter-et c) x) ((pass-time-through-counter x) c)]
                         [(= (counter-et c) x) ((pass-time-through-counter x) (remove-first-from-counter c))]
                         [else
                          (let
                              ((rest (- x (counter-et c))))
                            ((pass-time-through-counter rest) (remove-first-from-counter c))
                            )
                          ]
                         )
                       )
                     slow-counters)
                    exit-list)]
        
        )))