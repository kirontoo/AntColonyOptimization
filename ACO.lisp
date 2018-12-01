(DEFUN initAntColony (size)       ;creat colony with given size
    (SETQ antList ())
    (LOOP for count from 0 to size                              ;colony is (LIST ant ant ant)
        do
        (SETQ antList (APPEND antList (LIST (LIST 0 0 nil))))   ;ants are (LIST x y return)
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
            (IF (NTH 0 (NTH x (NTH y grid)))
                (FORMAT t "x")
                (FORMAT t "-")
            )
        )
        (FORMAT t "~%")
    )
)

(SETQ testGrid (LIST 
                (LIST ;(0,0)   (1,0)     (2,0)    (3,0)      (4,0)
                    (LIST t) (LIST nil) (LIST t) (LIST nil) (LIST t) )
                (LIST ;(1,0)   (1,1)     ...
                    (LIST t) (LIST t) (LIST t) (LIST nil) (LIST t) )
                (LIST ;(2,0)   ...
                    (LIST t) (LIST nil) (LIST t) (LIST nil) (LIST t) )
                )
)   ;idk what is a good way to make a 40x60 maze without hardcoding

(printGrid testGrid)

