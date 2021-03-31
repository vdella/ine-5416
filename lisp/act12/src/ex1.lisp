(defun sumOf (elements)
    (if (null elements)
        0
        (+ (first elements) (sumOf (rest elements)))
    )
)

(defun main()
    (write-line (write-to-string (sumOf (list 2 3 7 9 4))))
    (write-line (write-to-string (sumOf (list 10 10 10 10))))
)

(main)