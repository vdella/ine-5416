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


(defvar max-in-line 5)  ; Deve ser alterado de acordo com o tamanho da linha de cada matriz dada.
(defvar max-in-board (* max-in-line max-in-line))


(defun region-board ()
    regions
)


; Converte um índice da matriz para um ponto (i, j).
(defun i-to-point (i)
    (make-point
        :i (floor (/ i max-in-line))
        :j (mod i max-in-line)))


; Converte um ponto para um índice da matriz.
(defun point-to-i (pnt)
    (+ (point-j pnt) (* (point-i pnt) max-in-line))
)


(defun point-to-i-from (i j)
    (+ j (* i max-in-line))
)


; Retorna uma lista com os valores adjacentes ao índice i.
(defun adj-row (i brd)
    (setq actual (i-to-point i))
    (setq row (point-i actual))

    (cond
        ((= 0 row) 
            (append (adj-from-i i (in-row (+ row 1) brd 0) 0) ; Adjacentes da linha abaixo.
                    (adj-from-i i (in-row row brd 0) 0))      ; Adjacentes da mesma linha (inclui i).
        )

        ((= (- max-in-line 1) row) 
            (append (adj-from-i i (in-row (- row 1) brd 0) 0) ; Adjacentes da linha acima.
                    (adj-from-i i (in-row row brd 0) 0))      ; Adjacentes da mesma linha (inclui i).
        )

        (t 
            (append (adj-from-i i (in-row (+ row 1) brd 0) 0) ; Adjacentes da linha abaixo.
                    (adj-from-i i (in-row (- row 1) brd 0) 0) ; Adjacentes da linha acima.
                    (adj-from-i i (in-row row brd 0) 0))      ; Adjacentes da mesma linha.
        )
    )
)


; Dado um índice i (convertido para um ponto (i, j)), uma linha adjacente row e um índice inicial (que deve ser 0),
; retornam-se as células adjacentes a (i, j) nesta linha.
(defun adj-from-i (i row start)
    (setq pnt (i-to-point i))
    (cond
        ((null row) 
            '()
        )

        ((= start (- (point-j pnt) 1)) 
            (cons (car row) (adj-from-i i (cdr row) (+ 1 start)))
        )

        ((= start (+ (point-j pnt) 1)) 
            (cons (car row) (adj-from-i i (cdr row) (+ 1 start)))
        )

        ((= start (point-j pnt)) 
            (cons (car row) (adj-from-i i (cdr row) (+ 1 start)))
        )

        (t 
            (adj-from-i i (cdr row) (+ 1 start))
        )
    )
)


(defun in-region (region-index rgns brd)
    (cond
        ((null brd) 
            '()  ; Chegou-se ao fim do board.
        )

        ((= region-index (car rgns)) 
            (cons (car brd) (in-region region-index (cdr rgns) (cdr brd)))  ; Estamos na região desejada. Adiciona-se à lista.
        )
        
        (t 
            (in-region region-index (cdr rgns) (cdr brd))  ; Caso contrário, pula-se à próxima posição.
        )
    )
)  


; Retorna o tamanho da região.
(defun region-length (r rgns brd)
    (length (in-region r rgns brd))
)


; Remove da lista todas as ocorrências de um dado valor.
(defun remove-all (val lista)
    (cond
        ((null lista) 
            '()
        )

        ((= val (car lista))
            (remove-all val (cdr lista))
        )

        (t 
            (cons (car lista) (remove-all val (cdr lista)))
        )
    )
)


; Remove da primeira lista os valores presentes na segunda.
(defun remove-val (lista1 lista2)
    (cond
        ((null lista1) 
            '()
        )

        ((null lista2)
            lista1
        )

        (t
            (remove-val (remove-all (car lista2) lista1) (cdr lista2))
        )
    )
)


; Cria uma lista de 1 até 'max'. No caso, adicionar o valor 'min' é
; opcional e este será interpretado como 1 por padrão. Itera 1 número de cada vez (step).
(defun inclusive-range (max &optional (min 1) (step 1))
    (loop for n from min to max by step
        collect n)
)


; Retorna row, busca a partir de v (que deve ser 0).
(defun in-row (row brd v)
    (setq actual (i-to-point v))

    (cond
        ((> (point-i actual) row) 
            '()  ; Passamos do índice i.
        )
        ((= (point-i actual) row) 
            (cons (car brd) (in-row row (cdr brd) (+ 1 v)))
        )

        (t 
            (in-row row (cdr brd) (+ 1 v))
        )
    )
) 


; Cria uma lista nova com valores de 1 a n, em reverso.
(defun reverse-range (n)
    (if (<= n 0)
        '()
        (cons n (reverse-range (- n 1)))
    )
)


; Retorna a lista de valores possiveis no índice i.
(defun possibles-at (i rgns brd)
    (setq used-values (append (adj-row i brd) (in-region (nth i rgns) rgns brd)))
    (setq r-length (region-length (nth i rgns) rgns brd))
    (cond
        ((> i (length brd)) 
            '()
        )

        ((= 0 (nth i brd)) 
            (remove-val (reverse-range r-length) used-values)
        )

        (t
            (nth i brd)
        )
    )
)


(defun try-insert (i brd value)
    (setf (nth i brd) value)
    brd
)


; Retorna o índice da próxima célula vazia.
(defun next-possible (brd)
    (if (= 0 (car brd))
        0
        (+ 1 (next-possible (cdr brd)))
    )
)


(defun solve (i rgns brd xs)
    (setq my-board (copy-list brd))
    (cond
        ((null xs)
            '()
        )

        ((and (= i (- max-in-board 1)) (null xs))
            '()
        )

        ((and (= i (- max-in-board 1)) (null (cdr xs)))
            (try-insert i my-board (car xs))
        )

        ((and (= i (- max-in-board 1))
            (not (null (car xs)))) '()
        )

        ((null (solve-ahead i rgns my-board xs))
            (solve i rgns brd (cdr xs))
        )

        (t
            my-board
        )
    )
)


(defun solve-next (rgns brd)
    (setq i (next-possible brd))
    (solve i rgns brd (possibles-at i rgns brd))
)


(defun solve-ahead (i rgns brd xs)
    (setq inserted (try-insert i brd (car xs)))
    (solve-next rgns inserted)
)


; Retorna a linha row do tabuleiro brd como string (ref é um contador e deve ser 0)
(defun get-row (row brd rgns ref)
  (setq actual (i-to-point ref))
  (setq curr-half (concatenate 'string (write-to-string (car brd)) "("))
  (setq curr-half2 (concatenate 'string curr-half (write-to-string (car rgns))))
  (setq curr (concatenate 'string curr-half2 ") "))
  (cond
    ((null brd) "")
    ((= row (point-i actual)) (concatenate 'string curr (get-row row (cdr brd) (cdr rgns) (+ 1 ref))))
    (t (get-row row (cdr brd) (cdr rgns) (+ 1 ref)))))


; Printa o tabuleiro brd, dado o maximo de linhas lines (cnt é um contador e deve se 0)
(defun print-board (brd rgns lines cnt)
  (write-line (get-row cnt brd rgns 0))
  (cond
    ((> cnt lines) ())
    (t (print-board brd rgns lines (+ 1 cnt)))))


(defun main ()
    (setq solved-board (solve 0 regions board (possibles-at 0 regions board)))
    (print-board solved-board regions max-in-line 0)
)

(main)
