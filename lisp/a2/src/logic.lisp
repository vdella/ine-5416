(setq board '(0 0 0 0 1
              3 0 0 0 0
              0 0 0 0 0
              5 0 0 1 0
              0 0 0 4 0))

(setq regions '(1 1 2 2 2
                3 3 3 2 2
                6 3 4 4 4
                6 6 6 4 4
                6 5 5 5 5))

(defstruct point i j)


(defun region-board ()
    regions
)

; Converte um índice da matriz para um ponto (i, j).
(defun itopoint (i)
  (make-point
    :i (floor (/ i 5))
    :j (mod i 5)))


; Converte um ponto para um índice da matriz.
(defun pointoi (pnt)
  (+ (point-j pnt) (* (point-i pnt) 5)))


; Retorna uma lista de valores na regiao dada
; r = regiao
; rgns = board de regioes
; brd = board
(defun inRegion (r rgns brd)
  (cond
    ((null brd) '())  ; chegamos ao fim do board
    ((= r (car rgns)) (cons (car brd) (inRegion r (cdr rgns) (cdr brd))))  ; estamos na regiao. Adicionamos na lista
    (t (inRegion r (cdr rgns) (cdr brd)))))  ; caso contrario, pulamos para a proxima posicao


; Retorna o tamanho da regiao
(defun regionLength (r)
  (length (inRegion r regions board)))


; Remove da lista todas as ocorrencias de um dado valor
(defun removeAll (val lista)
  (cond
    ((null lista) '())
    ((= val (car lista)) (removeAll val (cdr lista)))
    (t (cons (car lista) (removeAll val (cdr lista))))))


; Remove da primeira lista os valores presentes na segunda lista
(defun removeVal (lista1 lista2)
  (cond
    ((null lista1) '())
    ((null lista2) lista1)
    (t (removeVal (removeAll (car lista2) lista1) (cdr lista2)))))


(defun possiblesAt (i brd)
    (write-line (write-to-string i))
    (if (> i 24)
        ()  ; Retorna uma lista vazia, indicando index out of bounds.
        (if (/= (nth i brd) 0)
            (cons (nth i brd))
            (progn 
                (setq max-length (nth i regions))
                (setq to-be-filled ())
                (setq to-remove (create-list-in-range to-be-filled 1 max-length))
                ; TODO needs to remove values from to-remove list.
            )
        )
    )

    (defun create-list-in-range (returnable lower upper)
        (if (= lower upper)
            returnable
            (progn
                (append returnable lower)  ; possible error.
                (create-list-in-range returnable (+ 1 lower) upper)
            )
        )
    )
)


;; Retorna uma lista com os valores adjacentes ao indice i. Cnt serve como contador e deve comecar em 0
(defun adjRow (i brd)
  (setq actual (itopoint i))
  (setq row (point-i actual))

  (cond
    ((= 0 row) (inrow (+ row 1) brd 0))
    ((= (- 5 1) row) (inrow (- row 1) brd 0))
    (t (concat-lists (inrow (+ row 1) brd 0) (inrow (- row 1) brd 0)))))



;; Retorna a linha row, busca a partir de v (deve ser 0)
(defun inrow (row brd v)
  (setq actual (itopoint v))

  (cond
    ((> (point-i actual) row) '())     ;; passamos do indice i
    ((= (point-i actual) row) (cons (car brd) (inrow row (cdr brd) (+ 1 v))))
    (t (inrow row (cdr brd) (+ 1 v))))) 

(defun concat-lists (seq1 seq2)
  (if (null seq1)
      seq2
      (cons (car seq1) (concat-lists (cdr seq1) seq2))))

; Retorna o indice da proxima celula vazia 
(defun nextPossible (brd)
  (if (= 0 (car brd))
    0
    (+ 1 (nextPossible (cdr brd)))))


; Cria uma lista nova com valores de 1 a n em reverso
(defun nlist (n)
  (if (<= n 0)
    '()
    (cons n (nlist (- n 1)))))

(defun main()
    (write-line "Convertendo 7 para i e j")
    (write-line (write-to-string (itopoint 11)))
    (setq pnt (itopoint 14))
    (write-line (write-to-string pnt))
    (write-line (write-to-string (pointoi pnt)))
    (write-line (write-to-string (inRegion 4 regions board)))
    (write-line (write-to-string (regionLength 1)))
    (write-line (write-to-string (removeAll 3 '(2 1 0 3 9 4 3))))
    (write-line (write-to-string (removeVal '(2 1 0 3 9 4 3) '(2 3 9 3))))
    (write-line (write-to-string (nextPossible board)))
    (write-line (write-to-string (nlist 7))))
    (write-line "AAAA")
    (write-line (write-to-string (region-board)))
    (write-line (write-to-string (inrow 3 board 0)))
    (write-line "linhas adjacentes")
    (write-line (write-to-string (adjRow 7 board)))
    

(main)

