(defun smallestNumberOf (elements pivot)
    (if (null elements)
        pivot
        (if (<= (first elements) pivot)
            (smallestNumberOf (rest elements) (first elements))
            (smallestNumberOf (rest elements) pivot)
        )
    )
)

(defun main()
    (write-line (write-to-string (smallestNumberOf (list 2 3 7 9 4) 10)))
)

(main)