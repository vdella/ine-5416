(defun xorOf (a b)
    (if (and (or a (not b)) (or b (not a)))
        nil
        t
    )
)

(defun main()
    (setq a (read))
    (setq b (read))
    (write-line (write-to-string (xorOf a b)))
)

(main)