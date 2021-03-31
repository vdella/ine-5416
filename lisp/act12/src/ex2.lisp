(defun averageOf (elements)
    (/ (sumOf elements) (lengthOf elements))
)

(defun sumOf (elements)
    (if (null elements)
        0
        (+ (first elements) (sumOf (rest elements)))
    )
)

(defun lengthOf (elements)
    (if (null elements)
        0
        (+ 1 (lengthOf (rest elements)))
    )
)

(defun main()
    (write-line (write-to-string (averageOf (list 2 3 7 9 4))))
    (write-line (write-to-string (averageOf (list 10 10 10 10))))
)

(main)