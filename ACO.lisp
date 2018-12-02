;; constants
(defconstant SK 0.1) ;; for heuristic function

(DEFUN initAntColony (size)       ;creat colony with given size
    (SETQ antList ())
    (LOOP for count from 0 to size                              ;colony is (LIST ant ant ant)
        do
        (SETQ antList (APPEND antList (LIST (LIST (LIST 0 0) nil (LIST ())))))   ;ants are (LIST (LIST x y return) (LIST path))
    )
    (RETURN-FROM initAntColony antList)
)
(FORMAT t "ants(x y return):    ")
(PRINC (initAntColony 5))

(DEFUN printGrid (grid)
    (FORMAT t "~%_________~%")
    (LOOP for y from 0 to (- (LIST-LENGTH grid) 1)
        do
        (LOOP for x from 0 to (- (LIST-LENGTH (NTH y grid)) 1)
            do
            (IF (CAR (NTH x (NTH y grid)))
                (FORMAT t "x")
                (FORMAT t "-")
            )
        )
        (FORMAT t "~%")
    )
)

(SETQ testGrid (LIST    ;(y (x (wall scent) ) )
                (LIST ;(0,0)   (1,0)     (2,0)    (3,0)      (4,0)
                    (LIST t (FLOAT 0)) (LIST nil (FLOAT 0)) (LIST t (FLOAT 0)) (LIST nil (FLOAT 0)) (LIST t (FLOAT 0)) )
                (LIST ;(1,0)   (1,1)     ...
                    (LIST t (FLOAT 0)) (LIST t (FLOAT 0)) (LIST t (FLOAT 0)) (LIST nil (FLOAT 0)) (LIST t (FLOAT 0)) )
                (LIST ;(2,0)   ...
                    (LIST t (FLOAT 0)) (LIST nil (FLOAT 0)) (LIST t (FLOAT 0)) (LIST nil (FLOAT 0)) (LIST t (FLOAT 0)) )
                )
)   ;idk what is a good way to make a 40x60 maze without hardcoding

(printGrid testGrid)

(SETF gridStr "-x-w-g
xyzt
")

(DEFUN GRID-TO-LIST (str)
	(SETF row ())
	(SETF grid ())

	(LOOP for c across str
		IF (eq c #\newline)
		DO ( progn
			(SETF grid (APPEND grid (LIST row)))
			(SETF row ())		;; clear row
		)
		ELSE DO (SETF row (APPEND row (LIST (LIST (STRING c) (FLOAT 0)))))
	)

	(PRINC row)
	(PRINT grid)

	(RETURN-FROM GRID-TO-LIST grid)
)

(SETF grid-list (GRID-TO-LIST gridStr))

(DEFUN deltaMax (a b) ;; each param is a coordinate (x y)
	(RETURN-FROM deltaMax (ABS (- (MAX (CAR a) (CADR a)) (MAX (CAR b) (CADR b)))))
)

(DEFUN deltaSum (a b)
	(RETURN-FROM deltaSum (ABS (- (+ (CAR a) (CADR a)) (+ (CAR b) (CADR b)))))
)

(DEFUN getCell (x y grid) 
	(RETURN-FROM getCell (NTH y (NTH x grid)))
)

;; gets the scent
(PRINT (CADR (getCell 1 2 grid-list)))

(SETF mode "forage") ;; temp ant mode

;; cur = current cell, nbr = neighboring cell
(DEFUN getHeuristicVal (cur nbr) ;; (x y) coordinates
	(SETF *random-state* (make-random-state t))
	(SETF MC 0)
	(SETF cell (getCell (CAR cur) (CADR cur) grid-list))

	(IF (string= mode "forage")
		(SETF MC (deltaMax cur nbr))
		(SETF MC (deltaSum cur nbr))
	)

	(RETURN-FROM getHeuristicVal (+ MC (* SK (CADR cell)) (/ (- (RANDOM 161) 80) 100.0)))
)

(PRINT (getHeuristicVal (LIST 1 2) (LIST 1 1)))