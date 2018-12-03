;; constants
(defconstant SK 0.1) ;; for heuristic function
(defconstant E 0.1) ;; scent evaporation value for scent reduction

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

;; @param str: grid in string form
;; @return: a list ( () () ... )
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

;; @param a: coordinates (x y)
;; @param b: coordinates (x y)
;; @return: difference of max of coordinates
(DEFUN deltaMax (a b)
	(RETURN-FROM deltaMax (ABS (- (MAX (CAR a) (CADR a)) (MAX (CAR b) (CADR b)))))
)


;; @param a: coordinates (x y)
;; @param b: coordinates (x y)
;; @return: difference of sum of coordinates
(DEFUN deltaSum (a b)
	(RETURN-FROM deltaSum (ABS (- (+ (CAR a) (CADR a)) (+ (CAR b) (CADR b)))))
)

;; @param x: number
;; @param y: number
;; @return: a cell - (string float)
(DEFUN getCell (x y grid)
	(IF (OR (MINUSP x) (MINUSP y)) (RETURN-FROM getCell nil))
	(RETURN-FROM getCell (NTH y (NTH x grid)))
)

;; @param l: list
;; @param n: index position
;; @param elem: list to insert
;; @return: list
(DEFUN INSERT-N (l n elem)
    (COND
        ((NULL l) ())
        ((= n 0) (CONS elem l))
        (T (CONS (CAR l) (INSERT-N (CDR l) (- n 1) elem)))
    )
)

;; gets the scent
(PRINT (CADR (getCell 1 2 grid-list)))

;; @param cur: current cell coordinates (x y)
;; @param nbr: neighboring cell coordinates (x y)
;; @param mode: t or nil
;; @return: float - heuristice value
(DEFUN getHeuristicVal (cur nbr mode)
	(SETF *random-state* (make-random-state t))
	(SETF MC 0)
	(SETF cell (getCell (CAR cur) (CADR cur) grid-list))

	(IF mode
		(SETF MC (deltaMax cur nbr))
		(SETF MC (deltaSum cur nbr))
	)

	(RETURN-FROM getHeuristicVal (+ MC (* SK (CADR cell)) (/ (- (RANDOM 161) 80) 100.0)))
)

(PRINT (getHeuristicVal (LIST 1 2) (LIST 1 1) t))
(PRINT (getHeuristicVal (LIST 1 2) (LIST 1 1) nil))

;; @param: a cell from the grid
;; @return: float - scent reduction value
(DEFUN getSR (cell) 
	(IF (> 1 (CADR cell))
		(RETURN-FROM SR (FLOAT 0))
		(RETURN-FROM SR (* (CADR cell) E))
    )
)

;; @param srVal: float - scent reduction value
;; @param cell: list (string float)
;; @return: a cell with the new scent value
(DEFUN weakenScent (cell srVal) 
	(SETF newScent (- (CADR cell) srVal))
	(RETURN-FROM weakenScent (LIST (CAR cell) (FLOAT newScent)))
)

;; @param pos: coordinates of cell you want to replace (x y)
;; @param rCell: cell to replace with - list (string float)
;; @param g: grid
;; @return: new grid
(DEFUN replaceCell (pos rCell g) 
	(SETF nr (INSERT-N (REMOVE (getCell (CAR pos) (CADR pos) g) (NTH (CAR pos) g)) (CADR pos) rCell)) ;; insert rCell into the row
	(SETF ng (REMOVE (NTH (CAR pos) g) g)) ;; remove old row
	(RETURN-FROM replaceCell (INSERT-N ng (CAR pos) nr))
)

;; @param pos: coordinates of cell
;; @param val: val to deposit
;; @param g: grid
(DEFUN  depositScent (pos val g) 
	(RETURN-FROM depositScent (LIST (CAR (getCell (CAR pos) (CADR pos) g)) (FLOAT (+ (CADR (getCell (CAR pos) (CADR pos) g)) val))))
)