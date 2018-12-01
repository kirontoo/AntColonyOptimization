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

