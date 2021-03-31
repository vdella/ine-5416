(defun power (a b)
    (expt a b)
)

(defun main()
    (setq a (read))
    (setq b (read))
    (write-line (write-to-string (power a b)))
)

(main)