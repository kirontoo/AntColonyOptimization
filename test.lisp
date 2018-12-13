;; constants
(defconstant SK 0.1) ;; for heuristic function
(defconstant E 0.1) ;; scent evaporation value for scent reduction
(defconstant D 10) ;; amount of scent to deposit

;; Ant structure: ( (y x) MODE (Tabu list) (path list) )
(DEFUN initAnt ()
    (RETURN-FROM initAnt (LIST (LIST (LIST 0 0) T (LIST (LIST 0 0)) (LIST ()))))
)

(DEFUN printGrid (g)
    (FORMAT t "~%-----GRID-----~%")
    (LOOP for row from 0 to (- (LIST-LENGTH g) 1)
        do (PRINT (NTH row g))
     )
)

(SETF gridStr "
-----x-x--------x------x-----x-x-----x---------x---x----x---
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
		(RETURN-FROM getSR (FLOAT 0))
		(RETURN-FROM getSR (* (CADR cell) E))
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
		(SETF nr (APPEND (REMOVE (getCell (CAR pos) (CADR pos) g) (NTH (CAR pos) g)) (LIST rCell)))
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
        (IF (EQUAL "-" (CAR (getCell (- (CAR pos) 1) (CADR pos) gr)))
            (progn
			    (SETF cell ( depositScent (LIST (- (CAR pos) 1) (CADR pos) gr) depositVal gr))
			    (SETF gr (replaceCell (LIST (- (CAR pos) 1) (CADR pos)) cell gr))
		    )
        )
	)

	;; deposit to cell below
	(IF (getCell (+ (CAR pos) 1) (CADR pos) gr)
        (IF (EQUAL "-" (CAR (getCell (+ (CAR pos) 1) (CADR pos) gr)))
            (progn
                (SETF cell ( depositScent (LIST (+ (CAR pos) 1) (CADR pos) gr) depositVal gr))
                (SETF gr (replaceCell (LIST (+ (CAR pos) 1) (CADR pos)) cell gr))
            )
        )
	)
	;; deposit to cell on left
	(IF (getCell (CAR pos) (- (CADR pos) 1) gr)
        (IF (EQUAL "-" (CAR (getCell (CAR pos) (- (CADR pos) 1) gr)))
            (progn
                (SETF cell ( depositScent (LIST (CAR pos) (- (CADR pos) 1) gr) depositVal gr))
                (SETF gr (replaceCell (LIST (CAR pos) (- (CADR pos) 1)) cell gr))
            )
        )
	)
	;;deposit to cell on right
	(IF (getCell (CAR pos) (+ (CADR pos) 1) gr)
        (IF (EQUAL "-" (CAR (getCell (CAR pos) (+ (CADR pos) 1) gr)))
            (progn
                (SETF cell (depositScent (LIST (CAR pos) (+ (CADR pos) 1) gr) depositVal gr))
                (SETF gr (replaceCell (LIST (CAR pos) (+ (CADR pos) 1)) cell gr))
            )
        )
	)

	(RETURN-FROM depositToArea gr)
)

;; @param ant: current ant
;; @param newAnt: ant with new mode (return)
;; @param list: antList
;; @return: list of ants with updated mode
(DEFUN updateAntList (ant newAnt list) 
	(SETF n (POSITION ant list))
	(SETF list (REMOVE ant list :count 1) )
	(IF (= n (LIST-LENGTH list))
		(RETURN-FROM updateAntList (APPEND list (LIST newAnt)))
		(RETURN-FROM updateAntList (INSERT-N list n newAnt))
	)
)

;; set ant to return mode
(DEFUN flipAntMode (ant)
    (LIST (CAR ant) nil (CDR ant)) ;; flip to return mode
)

(DEFUN moveList (ant grid)  ;create list of possible moves
    (SETQ possibleMoves ())
    (IF (AND (getCell (NTH 0 (NTH 0 ant)) (- (NTH 1 (NTH 0 ant)) 1) grid)  ;check 0<x
            (STRING= "-" (NTH 0 (getCell (NTH 0 (NTH 0 ant)) (- (NTH 1 (NTH 0 ant)) 1) grid)))    ;check wall
            (NOT (MEMBER (LIST (NTH 0 (NTH 0 ant)) (- (NTH 1 (NTH 0 ant)) 1)) (NTH 2 ant) :test #'equal)))  ;check if in tabu
        
        (SETQ possibleMoves (APPEND possibleMoves (LIST (LIST
                    (getHeuristicVal (NTH 0 ant) (LIST (NTH 0 (NTH 0 ant)) (- (NTH 1 (NTH 0 ant)) 1)) (NTH 1 ant))
                    (NTH 0 (NTH 0 ant)) (- (NTH 1 (NTH 0 ant)) 1)))))
    )
    (IF (AND (getCell (NTH 0 (NTH 0 ant)) (+ (NTH 1 (NTH 0 ant)) 1) grid)  ;check x<39
            (STRING= "-" (NTH 0 (getCell (NTH 0 (NTH 0 ant)) (+ (NTH 1 (NTH 0 ant)) 1) grid)))
            (NOT (MEMBER (LIST (NTH 0 (NTH 0 ant)) (+ (NTH 1 (NTH 0 ant)) 1)) (NTH 2 ant) :test #'equal)))
        (SETQ possibleMoves (APPEND possibleMoves (LIST (LIST
                    (getHeuristicVal (NTH 0 ant) (LIST (NTH 0 (NTH 0 ant)) (+ (NTH 1 (NTH 0 ant)) 1)) (NTH 1 ant))
                    (NTH 0 (NTH 0 ant)) (+ (NTH 1 (NTH 0 ant)) 1)))))
    )
    (IF (AND (getCell (- (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant)) grid) ;check 0<y
            (STRING= "-" (NTH 0 (getCell (- (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant)) grid)))
            (NOT (MEMBER (LIST (- (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant))) (NTH 2 ant) :test #'equal)))
        
        (SETQ possibleMoves (APPEND possibleMoves (LIST (LIST
                    (getHeuristicVal (NTH 0 ant) (LIST (- (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant))) t)
                    (- (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant))))))
    )
    (IF (AND (getCell (+ (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant)) grid) ;check y<59
            (STRING= "-" (NTH 0 (getCell (+ (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant)) grid)))
            (NOT (MEMBER (LIST (+ (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant))) (NTH 2 ant) :test #'equal)))
        
        (SETQ possibleMoves (APPEND possibleMoves (LIST (LIST
                    (getHeuristicVal (NTH 0 ant) (LIST (+ (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant))) (NTH 1 ant))
                    (+ (NTH 0 (NTH 0 ant)) 1) (NTH 1 (NTH 0 ant))))))
    )
    (RETURN-FROM moveList possibleMoves )
)

(DEFUN moveAnt (ant grid)  ;(LIST (LIST x y) return (LIST path))         check l r u d
    (IF (AND (= (- (LIST-LENGTH grid) 1) (NTH 0 (NTH 0 ant)))     ;ant reached goal, set return bit
                    (= (- (LIST-LENGTH (NTH 0 grid)) 1) (NTH 1 (NTH 0 ant))))
        (PROGN 
            (SETF (NTH 1 ant) nil)
            (SETF (NTH 3 ant) (LIST (LIST 39 59)))
        )
    )
    (IF (NTH 1 ant)         ;check mode, forage or return
        (PROGN          ;forage
            (SETQ possibleMoves (moveList ant grid))
            (SETQ bestMove ())
            (SETQ chosenMove())
            (IF (= (LIST-LENGTH possibleMoves) 0)       ;no possible moves, go back
                (PROGN
                    (SETQ patternFound nil)                                         ;pattern match to allow ant to backtrack more than 1 cell
                    (LOOP for count from 0 to (- (LIST-LENGTH (NTH 2 ant)) 1)       ;backtrack more than 1
                        do
                        (IF (equal (NTH 1 (NTH 2 ant)) (NTH count (NTH 2 ant)))     ;find [decisionCell] in path
                            (IF (AND (equal (NTH 0 (NTH 2 ant)) (NTH (+ count 1) (NTH 2 ant))))
                                (PROGN
                                    (SETQ patternFound t)                           ;found repeating pattern of 2 cells. Go to cell before the 2 cells
                                    (SETQ chosenMove (LIST 0 (NTH 0 (NTH (+ count 2) (NTH 2 ant))) (NTH 1 (NTH (+ count 2) (NTH 2 ant))) ))
                                )
                            )
                        )
                    )
                    (IF patternFound  
                        (SETF (NTH 2 ant) (APPEND (LIST (LIST (NTH 1 chosenMove) (NTH 2 chosenMove))) (NTH 2 ant)))
                        (PROGN ;backtrack only 1
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
            (RETURN-FROM moveAnt (LIST (LIST (NTH 1 chosenMove) (NTH 2 chosenMove)) (NTH 1 ant) (NTH 2 ant) (LIST ())))
        )
        (PROGN      ;return
            (IF (> (LIST-LENGTH (NTH 2 ant)) 1) ;not back yet
                (PROGN
                    ;add scent
                    ;to forget dead ends, look for greatest nth occurence of cell, jump there
                    (SETQ nextMove ())
                    (SETQ leastRecentCount ())
                    (LOOP for count from 0 to (LIST-LENGTH (NTH 2 ant))
                        do
                        (IF (equal (NTH 1 (NTH 2 ant)) (NTH count (NTH 2 ant)))
                            (PROGN
                                (SETQ leastRecentCount count)
                            )
                        )
                    )
		            (RETURN-FROM moveAnt (LIST (NTH leastRecentCount (NTH 2 ant)) (NTH 1 ant) (NTHCDR leastRecentCount (NTH 2 ant)) (APPEND (LIST (NTH leastRecentCount (NTH 2 ant))) (NTH 3 ant))))
                    
                    ;for testing purposes, does not include best path (nth 3 ant)
                    ;(RETURN-FROM moveAnt (LIST (NTH leastRecentCount (NTH 2 ant)) (NTH 1 ant) (NTHCDR leastRecentCount (NTH 2 ant))))
                )
                (RETURN-FROM moveAnt ant)   ;made it back, do nothing
            )
        )
    )
)

;; ============MAIN LOOP===============
(SETF goalCount 0)
(SETF antColony (initAnt))
(SETF grid grid-list)
(SETF bestPath ())
(SETF doneList ())

(FORMAT t "STARTING PROGRAM~%~%")
;( LOOP WHILE ( < goalCount 1)
(LOOP WHILE (< (LIST-LENGTH doneList) 50)
    do
    (LOOP for n from 0 to (- (LIST-LENGTH antColony) 1)
        do
        ;; returning ants deposit scent to current position
        (IF (not (CADR (NTH n antColony)))
            (SETF grid (replaceCell (CAR (NTH n antColony)) (depositScent (CAR (NTH n antColony)) D grid) grid ))
        )   
        ;; ants move to target cell -> add to tabu list
        (IF (NOT (MEMBER doneList (NTH n antColony) :test #'equal))
            (PROGN
            (SETF movedAnt (moveAnt (NTH n antColony) grid))
            (SETF antColony (updateAntList (NTH n antColony) movedAnt antColony))
            )
        )

        ;; if on goal cell, start return journey
        ;;      and update best short path found
        (IF (equal (CAR (NTH n antColony)) (LIST (- (LIST-LENGTH grid) 1) (- (LIST-LENGTH (CAR grid)) 1)))
            (IF (OR (< (LIST-LENGTH (NTH 2 (NTH n antColony))) (LIST-LENGTH bestPath)) (= (LIST-LENGTH bestPath) 0))
            ;; (IF (< (LIST-LENGTH (NTH 3 (NTH n antColony))) (LIST-LENGTH bestPath))
                (PROGN
                    (SETF goalCount (+ goalCount 1))
                    (FORMAT t "reached goal. ant path length: ~a ~% current best path length: ~a~%" (LIST-LENGTH (NTH 2 (NTH n antColony))) (LIST-LENGTH bestPath))
                    (SETF bestPath (NTH 2 (NTH n antColony)))
                    (FORMAT t "new bestpath length: ~a ~%path: ~a~%" (LIST-LENGTH bestPath) bestPath)
                )
            )
        ) ;;TODO: change ant to return mode

        ;; check if ant is in return mode and have reached the starting point.

        ;; check if ant reached goal, put on on doneList
        (IF (AND (EQUAL (CAR (NTH n antColony)) (LIST (- (LIST-LENGTH grid) 1) (- (LIST-LENGTH (NTH 0 grid)) 1))) (CADR (NTH n antColony)))
            (PROGN
                (IF (NOT (MEMBER (NTH n antColony) doneList :test #'equal))
                    (PROGN
                        (FORMAT t "~%ant ~a is done. ants done: ~a~%" n (+ 1 (LIST-LENGTH doneList)))
                        ;; (FORMAT t "~%ant: ~a" (NTH n antColony))
                        (SETQ doneList (APPEND doneList (LIST (NTH n antColony))))
                    )
                )
        ;    (FORMAT t "~%~a is home~%" n)
        ;    (SETF antColony (REMOVE (NTH n antColony) antColony :count 1))
            )
        )
    )

    ;; For each grid cell
    (LOOP for y from 0 to (- (LIST-LENGTH grid) 1)
        do
        (LOOP for x from 0 to (- (LIST-LENGTH (NTH y grid)) 1)
            do
            (SETF cell (getCell y x grid))

            ;; compute scent reduction to all grid cells
            (SETF srVal (getSR cell))

            ;; each grid cell subtract SR from Scent value
            (SETF grid (replaceCell (LIST y x) (LIST (CAR cell) (FLOAT (- (CADR cell) srVal))) grid))

            ;; deposit 1/5 of srval to adjacent cells
            (SETF grid (depositToArea (LIST y x) srVal grid))
        )
    )

    ;; place new ant on starting cell
    ;;continue to place a new ant until 50 ants are on the grid
    (IF (> 50 (LIST-LENGTH antColony))
    	(SETF antColony (APPEND antColony (initAnt)))
    )
)
(FORMAT t "~%~% PROGRAM END.")