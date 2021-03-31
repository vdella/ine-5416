(defun absOf (n)
    (if (< n 0)
        (- n)
        n
    )    
)

(defun main()
    (setq a (read))
    (write-line (write-to-string (absOf a)))
)

(main)