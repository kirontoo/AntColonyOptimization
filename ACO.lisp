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

(DEFUN printGrid (g)
    (FORMAT t "~%-----GRID-----~%")
    (LOOP for row from 0 to (- (LIST-LENGTH g) 1)
        do (PRINT (NTH row g))
     )
)

(SETQ testGrid (LIST    ;(y (x (wall scent) ) )
                (LIST ;(0,0)                    (1,0)        (2,0)                (3,0)           (4,0)
                    (LIST t (FLOAT 0)) (LIST t (FLOAT 0)) (LIST t (FLOAT 0)) (LIST t (FLOAT 0)) (LIST t (FLOAT 0)) (LIST t (FLOAT 0)) )
                (LIST ;(1,0)   (1,1)     ...
                    (LIST t (FLOAT 0)) (LIST nil (FLOAT 0)) (LIST nil (FLOAT 0)) (LIST nil (FLOAT 0)) (LIST nil (FLOAT 0)) (LIST nil (FLOAT 0)))
                (LIST ;(2,0)   ...
                    (LIST t (FLOAT 0)) (LIST t (FLOAT 0)) (LIST t (FLOAT 0)) (LIST t (FLOAT 0)) (LIST t (FLOAT 0)) (LIST t (FLOAT 0)))
                )
)   ;idk what is a good way to make a 40x60 maze without hardcoding

(SETF gridStr "-----x-x--------x------x-----x-x-----x---------x---x----x---
--x--x-xxx--x---x-xx-xxxx-xx-x-------x-----x---x------x-x---
--x--x----x-x----------x-----x-xx-x-xx-----x---x---x--x-x---
--x--x-x--x-x---x-xxx-xx-----x-x-----xxx-x---------x--------
--x--x-x--x-x---x------xxxx--x-------------x-------x--x-x---
--x--x-x----x---x---x--x-------x-x-xx---x--xxxx--xxx--x-x---
--x----x--------x------xxxx----x-----x--x--x-------x--x-x---
-----x--xx--x---x---x--x-----x-xxx--xx-----xx-x-xxx-xx--x---
xx--xx-x----x-------x--------x-x-----x--x--x-------x--------
-------x----x---x---x--x-----x-x-----x--x--x-------x----x---
-----x-xxx--x---xxx-xx-x-xxx-xxxxxxxx-xxxxxxxxxx-xxxxx--x---
-----x-x----x---x------------x---------------------x----x---
xx-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-x
-------x---x----x------x----------x--x-----x--x-------------
--x----x---x----x------x----------x--x------------------x---
--xx--xx--------xx--xxxxx-xxxx-x--------x--x--x-xxx-x-------
--x--------xxx----x----x-------x--------x-----------x---x---
--x--------x----x-x-----xxxxxxxx-xxxxxxxxxxxxxxxxxxx-xxxxxxx
----x-xxxxxx--x---x----x--x-----x-------x-------------------
--x-----------x-x---x-xx--------xxxx--xxx-------------------
--x--------x--x-x-x----x--x-xxx------x--xxxxxxxxx-xxxxx-xxxx
--x--------x----x------x--x----------x----------------------
xxxxxxxx-xxxxxxxxxxxxx-x--x-----x-x-----x-------------------
---x----------x--x-----x--x-----x-x--x---xxxx--xx-xx-xxxxxxx
-----x-x--x-x-x--x-----x--xx--xxx-xx----x-------x--x---x----
-----x-------------x---x--x-----x----x--x-----x-x--------x--
---x---x--x-x------x---x--x-----xx--xx--x---x-x-x--x--------
xxxxxxxxxxxxxxxxxxxx--xx--------x----x--x---x---x------x-x--
---x--x--------------x-xxxx-xxxxxxxx-xxxxxxxxxxxxxxxxxxxxxxx
------x--------------x----x--x----x----------x-x--x---x-----
---x--xxxx-xxxx-xxxxx--x--x-------xxxxx-xx-----x--x---xx-x-x
---x--x----x-----x---x-x--x--x-------------x------x---x--x--
---x--xx-x-xx-xx-x-----x--x--xx--x-------x-x-x-x--x---x--x--
---x--x--------------x-x--x--x--x-xxxx-x-x-x-x-x------x--x--
---x--x--------------x-x-----x----x------x-x-x-x--x---x-----
---x----xxxxxx-xxxxxxx-x--x-------x------x-x-x-x--x---x--x--
---x--x--------------x-x--x--x--x-x--------x-x-x------------
------x---x--x---x---x-x---xxxx-xxxxxxxxxxx-xx----x---x--x--
---x--x--x----x--x---x-x--x------------------x-x--x---x--x--
---x-----x----x------x-x--x------------------x-x--x------x--
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

	(RETURN-FROM GRID-TO-LIST grid)
)

(SETF grid-list (GRID-TO-LIST gridStr))

;; @param a: coordinates (y x)
;; @param b: coordinates (y x)
;; @return: difference of max of coordinates
(DEFUN deltaMax (a b)
	(RETURN-FROM deltaMax (ABS (- (MAX (CAR a) (CADR a)) (MAX (CAR b) (CADR b)))))
)


;; @param a: coordinates (y x)
;; @param b: coordinates (y x)
;; @return: difference of sum of coordinates
(DEFUN deltaSum (a b)
	(RETURN-FROM deltaSum (ABS (- (+ (CAR a) (CADR a)) (+ (CAR b) (CADR b)))))
)

;; @param x: number
;; @param y: number
;; @return: a cell - (string float)
(DEFUN getCell (y x grid)
	(IF (OR (MINUSP y) (MINUSP x)) (RETURN-FROM getCell nil))
	(RETURN-FROM getCell (NTH x (NTH y grid)))
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

;; @param cur: current cell coordinates (y x)
;; @param nbr: neighboring cell coordinates (y x)
;; @param mode: t for forage or nil for return mode
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

;; @param pos: coordinates of cell you want to replace (y x)
;; @param rCell: cell to replace with - list (string float)
;; @param g: grid
;; @return: new grid
(DEFUN replaceCell (pos rCell g) 
	 ;; insert rCell into the row
	(IF (= (CADR pos) (- (LIST-LENGTH (NTH (CAR pos) g)) 1))
		(SETF nr (APPEND (REMOVE (getCell (CAR pos) (CADR pos) g) (NTH (CAR pos) g)) rCell))
		(SETF nr (INSERT-N (REMOVE (getCell (CAR pos) (CADR pos) g) (NTH (CAR pos) g)) (CADR pos) rCell))
	)

	(SETF ng (REMOVE (NTH (CAR pos) g) g)) ;; remove old row

	;; insert row into grid
	(IF (= (CAR pos) (LIST-LENGTH ng))
		(RETURN-FROM replaceCell (APPEND ng (LIST nr)))
		(RETURN-FROM replaceCell (INSERT-N ng (CAR pos) nr))
	)
)

;; @param pos: coordinates of cell
;; @param val: val to deposit
;; @param g: grid
;; @return: cell with updated scent
(DEFUN  depositScent (pos val g) 
	(RETURN-FROM depositScent (LIST (CAR (getCell (CAR pos) (CADR pos) g)) (FLOAT (+ (CADR (getCell (CAR pos) (CADR pos) g)) val))))
)

;; @param pos: coordinate of current cell
;; @param srVal: scent reduction value
;; @param gr: grid
;; @return: grid with updated scent
(DEFUN depositToArea (pos srVal gr)
	(SETF depositVal (FLOAT (/ srVal 5)))

	;; deposit to cell above (y-1 x)
	(IF (getCell (- (CAR pos) 1) (CADR pos) gr)
		(progn
			(SETF cell( depositScent (LIST (- (CAR pos) 1) (CADR pos) g) depositVal gr))
			(SETF gr (replaceCell (LIST (- (CAR pos) 1) (CADR pos)) cell gr))
		)
	)

	;; deposit to cell below
	(IF (getCell (+ (CAR pos) 1) (CADR pos) gr)
		(progn
			(SETF cell( depositScent (LIST (+ (CAR pos) 1) (CADR pos) g) depositVal gr))
			(SETF gr (replaceCell (LIST (+ (CAR pos) 1) (CADR pos)) cell gr))
		)
	)

	;; deposit to cell on left
	(IF (getCell (CAR pos) (- (CADR pos) 1) gr)
		(progn
			(SETF cell( depositScent (LIST (CAR pos) (- (CADR pos) 1) g) depositVal gr))
			(SETF gr (replaceCell (LIST (CAR pos) (- (CADR pos) 1)) cell gr))
		)
	)

	;;deposit to cell on right
	(IF (getCell (CAR pos) (+ (CADR pos) 1) gr)
		(progn
			(SETF cell( depositScent (LIST (CAR pos) (+ (CADR pos) 1) g) depositVal gr))
			(SETF gr (replaceCell (LIST (CAR pos) (+ (CADR pos) 1)) cell gr))
		)
	)

	(RETURN-FROM depositToArea gr)
)

;; @param ant: current ant
;; @param newAnt: ant with new mode (return)
;; @param list: antList
;; @return: list of ants with updated mode
(DEFUN updateAntList (ant newAnt list) 
    (SETF list (REMOVE ant list :count 1) )
    (RETURN-FROM updateAntList (APPEND list newAnt))
)

;; set ant to return mode
(DEFUN flipAntMode (ant)
    (LIST (CAR ant) nil (CDR ant)) ;; flip to return mode
)

(DEFUN moveAnt (ant grid)  ;(LIST (LIST x y) return (LIST path))         check l r u d
    (IF (NTH 1 ant)         ;check mode, forage or return
        (PROGN          ;forage
            (SETQ possibleMoves ()) ;(heuristic y x )        (getCell y x grid)
            (IF (AND (getCell (NTH 0 (NTH 0 ant)) (- (NTH 1 (NTH 0 ant)) 1) grid)  ;check 0<x
                    (NTH 0 (getCell (NTH 0 (NTH 0 ant)) (- (NTH 1 (NTH 0 ant)) 1) grid))    ;check wall
                    (NOT (MEMBER (LIST (NTH 0 (NTH 0 ant)) (- (NTH 1 (NTH 0 ant)) 1)) (NTH 2 ant) :test #'equal)))  ;check if in tabu
                
                (SETQ possibleMoves (APPEND possibleMoves (LIST (LIST
                            (getHeuristicVal (NTH 0 ant) (LIST (NTH 0 (NTH 0 ant)) (- (NTH 1 (NTH 0 ant)) 1)) (NTH 1 ant))
                            (NTH 0 (NTH 0 ant)) (- (NTH 1 (NTH 0 ant)) 1)))))
            )
            (IF (AND (getCell (NTH 0 (NTH 0 ant)) (+ (NTH 1 (NTH 0 ant)) 1) grid)  ;check x<39
                    (NTH 0 (getCell (NTH 0 (NTH 0 ant)) (+ (NTH 1 (NTH 0 ant)) 1) grid))
                    (NOT (MEMBER (LIST (NTH 0 (NTH 0 ant)) (+ (NTH 1 (NTH 0 ant)) 1)) (NTH 2 ant) :test #'equal)))
                (SETQ possibleMoves (APPEND possibleMoves (LIST (LIST
                            (getHeuristicVal (NTH 0 ant) (LIST (NTH 0 (NTH 0 ant)) (+ (NTH 1 (NTH 0 ant)) 1)) (NTH 1 ant))
                            (NTH 0 (NTH 0 ant)) (+ (NTH 1 (NTH 0 ant)) 1)))))
            )
            (IF (AND (getCell (- (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant)) grid) ;check 0<y
                    (NTH 0 (getCell (- (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant)) grid))
                    (NOT (MEMBER (LIST (- (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant))) (NTH 2 ant) :test #'equal)))
                
                (SETQ possibleMoves (APPEND possibleMoves (LIST (LIST
                            (getHeuristicVal (NTH 0 ant) (LIST (- (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant))) t)
                            (- (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant))))))
            )
            (IF (AND (getCell (+ (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant)) grid) ;check y<59
                    (NTH 0 (getCell (+ (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant)) grid))
                    (NOT (MEMBER (LIST (+ (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant))) (NTH 2 ant) :test #'equal)))
                
                (SETQ possibleMoves (APPEND possibleMoves (LIST (LIST
                            (getHeuristicVal (NTH 0 ant) (LIST (+ (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant))) (NTH 1 ant))
                            (+ (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant))))))
            )
            (SETQ bestMove ())
            (SETQ chosenMove())
            (IF (= (LIST-LENGTH possibleMoves) 0)       ;no possible moves, go back
                (PROGN
                    (SETQ patternFound nil)                                         ;pattern match to allow ant to backtrack more than 1 cell
                    (LOOP for count from 0 to (- (LIST-LENGTH (NTH 2 ant)) 1)       ;backtrack more than 1
                        do
                        (IF (AND (NOT patternFound) (equal (NTH 1 (NTH 2 ant)) (NTH count (NTH 2 ant)))  )   ;find [decisionCell] in path
                            (IF (equal (NTH 0 (NTH 2 ant)) (NTH (+ count 1) (NTH 2 ant)))
                                (PROGN
                                    (SETQ patternFound t)                           ;found repeating pattern of 2 cells. Go to cell before the 2 cells
                                    (SETQ chosenMove (LIST 0 (NTH 0 (NTH (+ count 2) (NTH 2 ant))) (NTH 1 (NTH (+ count 2) (NTH 2 ant))) ))
                                    (SETF (NTH 2 ant) (APPEND (LIST (LIST (NTH 0 (NTH (+ count 2) (NTH 2 ant))) (NTH 1 (NTH (+ count 2) (NTH 2 ant))))) (NTH 2 ant)))
                                )
                            )
                        )
                    )
                    (IF (NOT patternFound)  ;backtrack only 1
                        (PROGN
                            (SETQ chosenMove (LIST 0 (NTH 0 (NTH 1 (NTH 2 ant))) (NTH 1 (NTH 1 (NTH 2 ant)))))
                            (SETF (NTH 2 ant) (APPEND (LIST (LIST (NTH 0 (NTH 1 (NTH 2 ant))) (NTH 1 (NTH 1 (NTH 2 ant))))) (NTH 2 ant)))
                        )
                    )
                )
                (PROGN 
                    (SETQ chosenMove (NTH 0 possibleMoves))     ;starts chosenMove at first of possibleMoves
                    (LOOP for count from 0 to (- (LIST-LENGTH possibleMoves) 1) ;compare with rest of possibleMoves
                        do                                                      ;set chosenMove to (highest heuristic y x)
                        (IF (< (NTH 0 chosenMove) (NTH 0 (NTH count possibleMoves)))   
                            (SETQ chosenMove (NTH count possibleMoves))
                        )
                    )
                    (SETF (NTH 2 ant) (APPEND (LIST (LIST (NTH 1 chosenMove) (NTH 2 chosenMove))) (NTH 2 ant))) ;append old position to front of tabu
                )   
            )
            (IF (AND (= 59 (NTH 0 (NTH 0 ant)))     ;ant reached goal, set return bit
                    (= 39 (NTH 1 (NTH 0 ant))))
                (SETF (NTH 1 ant) nil)
            )
            (RETURN-FROM moveAnt (LIST (LIST (NTH 1 chosenMove) (NTH 2 chosenMove)) (NTH 1 ant) (NTH 2 ant)))
        )
        ;(PROGN      ;return
            ;go to previous cell
            ;add scent
        ;)
    )
)       
(SETQ testAnt (LIST (LIST 0 0) t (LIST (LIST 0 0))))
(FORMAT t "~%move0: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move1: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move2: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move3: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move4: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move5: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move6: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move7: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move8: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move9: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move0: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move1: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move2: ~a" testAnt)
(SETQ testAnt (moveAnt testAnt testGrid))
(FORMAT t "~%move3: ~a" testAnt)
