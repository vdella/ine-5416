(setq board '(0 0 0 0 1
              3 0 0 0 0
              0 0 0 0 0
              5 0 9 1 9
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
(defun i-to-point (i)
  (make-point
    :i (floor (/ i 5))
    :j (mod i 5)))


; Converte um ponto para um índice da matriz.
(defun point-to-i (pnt)
  (+ (point-j pnt) (* (point-i pnt) 5))
)

(defun point-to-i-from (i j)
  (+ j (* i 5))
)

; Retorna uma lista com os valores adjacentes ao índice i. Cnt serve como contador e deve comecar em 0
(defun adj-row (i brd)
  (setq actual (i-to-point i))
  (setq row (point-i actual))

  (cond
    ((= 0 row) 
      (append (adj-from-i i (in-row (+ row 1) brd 0) 0) ; Adjacentes da linha abaixo
              (adj-from-i i (in-row row brd 0) 0))      ; Adjacentes da mesma linha (inclui i)
    )
    ((= (- 5 1) row) 
      (append (adj-from-i i (in-row (- row 1) brd 0) 0) ; Adjacentes da linha acima
              (adj-from-i i (in-row row brd 0) 0))      ; Adjacentes da mesma linha (inclui i)
    )
    (t 
      (append (adj-from-i i (in-row (+ row 1) brd 0) 0) ; Adjacentes da linha abaixo
              (adj-from-i i (in-row (- row 1) brd 0) 0) ; Adjacentes da linha acima
              (adj-from-i i (in-row row brd 0) 0))      ; Adjacentes da mesma linha
    )
  )
)


; Dado um indice i (convertido para um ponto (i, j)), uma linha adjacente row e um indice incial (deve ser 0),
; retorna as celulas adjacentes a (i, j) nessa linha
(defun adj-from-i (i row start)
  (setq pnt (i-to-point i))
  (cond
    ((null row) '())
    ((= start (- (point-j pnt) 1)) (cons (car row) (adj-from-i i (cdr row) (+ 1 start))))
    ((= start (+ (point-j pnt) 1)) (cons (car row) (adj-from-i i (cdr row) (+ 1 start))))
    ((= start (point-j pnt)) (cons (car row) (adj-from-i i (cdr row) (+ 1 start))))
    (t (adj-from-i i (cdr row) (+ 1 start)))))


(defun adj-col (i brd)
  (defun operator (i j)
    (cond
      ((< (- i 1) 0)
        (cond
          ((> (+ j 1) (- 5 1))
            (list
              (nth (point-to-i-from (+ i 1) j) board)
              (nth (point-to-i-from (+ i 1) (- j 1)) board)
            )
          )
          ((< (- j 1) 0)
            (list
              (nth (point-to-i-from (+ i 1) j) board)
              (nth (point-to-i-from (+ i 1) (+ j 1)) board)
            )
          )
          (t 
            (list
              (nth (point-to-i-from (+ i 1) j) board)
              (nth (point-to-i-from (+ i 1) (+ j 1)) board)
              (nth (point-to-i-from (+ i 1) (- j 1)) board)
            )
          )
        )
      )
      ((> (+ i 1) (- 5 1))
        (cond
          ((> (+ j 1) (- 5 1))
            (list
              (nth (point-to-i-from (- i 1) j) board)
              (nth (point-to-i-from (- i 1) (- j 1)) board)
            )
          )
          ((< (- j 1) 0)
            (list
              (nth (point-to-i-from (- i 1) j) board)
              (nth (point-to-i-from (- i 1) (+ j 1)) board)
            )
          )
          (t 
            (list
              (nth (point-to-i-from (- i 1) j) board)
              (nth (point-to-i-from (- i 1) (+ j 1)) board)
              (nth (point-to-i-from (- i 1) (- j 1)) board)
            )
          )
        )
      )
      ((< (- j 1) 0)
        (list
              (nth (point-to-i-from (+ i 1) j) board)
              (nth (point-to-i-from (+ i 1) (+ j 1)) board)
              (nth (point-to-i-from (- i 1) j) board)
              (nth (point-to-i-from (- i 1) (+ j 1)) board)
        )
      )
      ((> (+ j 1) (- 5 1))
        (list
              (nth (point-to-i-from (+ i 1) j) board)
              (nth (point-to-i-from (+ i 1) (- j 1)) board)
              (nth (point-to-i-from (- i 1) j) board)
              (nth (point-to-i-from (- i 1) (- j 1)) board)
        )
      )
      (t 
        (list
              (nth (point-to-i-from (+ i 1) j) board)
              (nth (point-to-i-from (- i 1) j) board)
              (nth (point-to-i-from (+ i 1) (+ j 1)) board)
              (nth (point-to-i-from (+ i 1) (- j 1)) board)
              (nth (point-to-i-from (- i 1) (+ j 1)) board)
              (nth (point-to-i-from (- i 1) (- j 1)) board)
        )
      )
    )
  )

  (setq actual (i-to-point i))
  (setq row (point-i actual))
  (setq col (point-j actual))
  (operator row col)
)


; Retorna uma lista de valores na regiao dada
; r = regiao
; rgns = board de regioes
; brd = board
(defun in-region (r rgns brd)
  (cond
    ((null brd) '())  ; chegamos ao fim do board
    ((= r (car rgns)) (cons (car brd) (in-region r (cdr rgns) (cdr brd))))  ; estamos na regiao. Adicionamos na lista
    (t (in-region r (cdr rgns) (cdr brd)))  ; caso contrario, pulamos para a proxima posicao
  )
)  


; Retorna o tamanho da regiao
(defun region-length (r rgns brd)
  (length (in-region r rgns brd))
)


; Remove da lista todas as ocorrencias de um dado valor
(defun remove-all (val lista)
  (cond
    ((null lista) '())
    ((= val (car lista)) (remove-all val (cdr lista)))
    (t (cons (car lista) (remove-all val (cdr lista))))
  )
)


; Remove da primeira lista os valores presentes na segunda lista
(defun remove-val (lista1 lista2)
  (cond
    ((null lista1) '())
    ((null lista2) lista1)
    (t (remove-val (remove-all (car lista2) lista1) (cdr lista2)))
  )
)


; Cria uma lista de 1 até 'max'. No caso, adicionar o valor 'min' é
; opcional e este será interpretado como 1 por padrão. Itera
; 1 número de cada vez (step).
(defun inclusive-range (max &optional (min 1) (step 1))
   (loop for n from min to max by step
      collect n)
)

; Retorna a linha row, busca a partir de v (deve ser 0)
(defun in-row (row brd v)
  (setq actual (i-to-point v))

  (cond
    ((> (point-i actual) row) '())     ;; passamos do indice i
    ((= (point-i actual) row) (cons (car brd) (in-row row (cdr brd) (+ 1 v))))
    (t (in-row row (cdr brd) (+ 1 v)))
  )
) 

; Cria uma lista nova com valores de 1 a n em reverso
(defun reverse-range (n)
  (if (<= n 0)
    '()
    (cons n (reverse-range (- n 1)))
  )
)

; Retorna a lista de valores possiveis no indice i
(defun possibles-at (i rgns brd)
  (setq used-values (append (adj-row i brd) (in-region (nth i rgns) rgns brd)))
  (setq r-length (region-length (nth i rgns) rgns brd))
  ; (write-line (write-to-string r-length))
  (cond
    ((> i (length brd)) '())
    ((= 0 (nth i brd)) (remove-val (reverse-range r-length) used-values))
    (t (nth i brd))))

(defun try-insert (i brd value)
    (setf (nth i brd) value)
    brd
)


; Retorna o indice da proxima celula vazia 
(defun next-possible (brd)
  (if (= 0 (car brd))
    0
    (+ 1 (next-possible (cdr brd)))
  )
)

(defun main()
    ; (write-line "Convertendo 7 para i e j")
    ; (write-line (write-to-string (i-to-point 11)))
    (setq pnt (i-to-point 17))
    ; (write-line (write-to-string pnt))
    ; (write-line (write-to-string (point-to-i pnt)))
    ; (write-line (write-to-string (in-region 4 regions board)))
    ; (write-line (write-to-string (region-length 1)))
    ; (write-line (write-to-string (remove-all 3 '(2 1 0 3 9 4 3))))
    ; (write-line (write-to-string (remove-val '(2 1 0 3 9 4 3) '(2 3 9 3))))
    ; (write-line (write-to-string (next-possible board)))
    ; (write-line (write-to-string (reverse-range 7))))
    ; (write-line (write-to-string (region-board)))
    ; (write-line (write-to-string (in-row 3 board 0)))
    ; (write-line "celulas adjacentes ao indice 7")
    (write-line (write-to-string (adj-row 11 board)))

    ; (write-line "AAAA")
    (write-line (write-to-string (adj-col 1 board)))
    (write-line (write-to-string (adj-col 7 board)))
    (write-line "AAAA")
    (write-line (write-to-string (possibles-at 18 regions board)))

    (setq row (point-i pnt))

    ; (write-line (write-to-string (adj-from-i 1 row 0)))
    ; (write-line (write-to-string (adj-from-i 7 row 0)))
    ; (setq adj (adj-row 20 board))

    ; (adj-from-i 1 (in-row (+ row 1) brd 0) 0)
    ; (adj-from-i 1 (in-row (+ row 1) brd 0) 0)
)
(main)

