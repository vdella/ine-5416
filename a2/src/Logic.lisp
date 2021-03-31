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


;; Converte um indice da matriz para um ponto (i, j)
(defun itopoint (i)
  (make-point
    :i (floor (/ i 5))
    :j (mod i 5)))


;; Converte um ponto para um indice da matriz
(defun pointoi (pnt)
  (+ (point-j pnt) (* (point-i pnt) 5)))


;; Retorna uma lista de valores na regiao dada
;; r = regiao
;; rgns = board de regioes
;; brd = board
(defun inRegion (r rgns brd)
  (cond
    ((null brd) '()) ;; chegamos ao fim do board
    ((= r (car rgns)) (cons (car brd) (inRegion r (cdr rgns) (cdr brd)))) ;; estamos na regiao. Adicionamos na lista
    (t (inRegion r (cdr rgns) (cdr brd))))) ;; caso contrario, pulamos para a proxima posicao


;; Retorna o tamanho da regiao
(defun regionLength (r)
  (length (inRegion r regions board)))


;; Remove da lista todas as ocorrencias de um dado valor
(defun removeAll (val lista)
  (cond
    ((null lista) '())
    ((= val (car lista)) (removeAll val (cdr lista)))
    (t (cons (car lista) (removeAll val (cdr lista))))))


;; Remove da primeira lista os valores presentes na segunda lista
(defun removeVal (lista1 lista2)
  (cond
    ((null lista1) '())
    ((null lista2) lista1)
    (t (removeVal (removeAll (car lista2) lista1) (cdr lista2)))))


(defun main()
  ;; nth n list = retorna o n elemento da lista.
  (write-line (write-to-string (nth 4 board)))
  (write-line "Convertendo 7 para i e j")
  (write-line (write-to-string (itopoint 11)))
  (setq pnt (itopoint 14))
  (write-line (write-to-string pnt))
  (write-line (write-to-string (pointoi pnt)))
  (write-line (write-to-string (inRegion 4 regions board)))
  (write-line (write-to-string (regionLength 1)))
  (write-line (write-to-string (removeAll 3 '(2 1 0 3 9 4 3))))
  (write-line (write-to-string (removeVal '(2 1 0 3 9 4 3) '(2 3 9 3)))))

(main)

