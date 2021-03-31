(defstruct point x y)

(defun distanceBetween (p1 p2)
    (setq abs-x (expt (- (point-x p1) (point-x p2)) 2))
    (setq abs-y (expt (- (point-y p1) (point-y p2)) 2))
    (sqrt (+ abs-x abs-y))
)

(defun are)

(defun main () 
    (setq point1 
        (
            make-point
                :x 2
                :y 2
        )
    )

    (setq point2 
        (
            make-point
                :x 2
                :y 2
        )
    )
    (write-line (write-to-string (distanceBetween point1 point2)))
)

(main)