(defun calculateAreaFrom (base height)
    (/ (* base height) 2)
)

(defun main()
    (setq base (read))
    (setq height (read))
    (write-line (write-to-string (calculateAreaFrom base height)))
)

(main)