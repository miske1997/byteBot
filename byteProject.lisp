(setq karakteri (list 'A 'B 'C 'D 'E 'F 'G 'H 'I 'J 'K 'L 'M 'N 'O 'P 'Q 'R 'S 'T 'U 'V 'W 'X 'Y 'Z))
(defvar putevi '())



(defvar whosPlaying 'O)
(defvar whosPlayingFirst 'X)
(defvar whoIsAI 'O)
(defvar whoAmI 'X)
(defvar sizeOfMatrix '0)

(defvar currentPlay '())
(defvar fromPlay '())
(defvar toPlay '())
(defvar heightPlay '())

(defvar finalStack '())
(defvar victor '())

(defun makeGraph(m n)(cond ((< (+ (* m m) m) n) (format t "~%") )
    ( (<= (floor (/ n m)) 1 ) (cons (makeCvor (1- n) m t)   (makeGraph m (+ n 2)) ))
    ((>= (floor (/ n m)) m) (cons (makeCvor n m t)  (makeGraph m (+ n 2)) ))
    ( ( eq (1+ (mod n m )) m) 
    (cons (makeCvor n m nil) (makeGraph m (+ n 1))  )  )
   ( ( eq (1+ (mod n m)) (- m 1)) 
    (cons (makeCvor n m nil) (makeGraph m (+ n 3))  )  )
    (t (cons (makeCvor n m nil) (makeGraph m (+ n 2))  ))))


(defun umetni(n m)(list 
 (if (>= (1- (floor (/ n m))) 1) (if (< (1- (1+ (mod n m))) 1) 0 ( + (* (1- (floor (/ n m))) 100) (1- (1+ (mod n m))) ))  0)
(if (>= (1- (floor (/ n m))) 1) (if (> (1+ (1+ (mod n m))) m) 0 ( + (* (1- (floor (/ n m))) 100) (1+ (1+ (mod n m))) )) 0)
(if (<= (1+ (floor (/ n m))) m) (if (> (1+ (1+ (mod n m))) m) 0 ( + (* (1+ (floor (/ n m))) 100) (1+ (1+ (mod n m))) )) 0)
(if (<= (1+ (floor (/ n m))) m) (if (< (1- (1+ (mod n m))) 1) 0 ( + (* (1+ (floor (/ n m))) 100) (1- (1+ (mod n m))) )) 0)
     ))





(defun makeCvor(n m empty)(cons (+ (* 100 (floor (/ n m))) (1+ (mod n m)))  (if (eq empty t)   (cons (list 'E)  (list (umetni n m)))  (cons  (if (eq (1+ (mod (floor (/ n m)) 2)) 1 ) (list 'X 'E) (list 'O 'E)) 
  (list(umetni n m)) ))) )


(defun playMove( from to n graph)( playAndRemoveMove from to n graph (takeMove from n graph)))


(defun grabN(li n)(cond ((null li) '())
                                   ((eq n 0) '())
                                   (t (cons (car li) (grabN (cdr li) (1- n))) )))

(defun removeN(li n)(cond ((null li) '())
                                   ((eq n 0) li)
                                   (t (removeN (cdr li) (1- n)) )))

(defun takeMove(from n graph)(cond ((null graph) '())
                                                ((eq (caar graph) from) (grabN (cadar graph) (- (length (cadar graph) ) n )))
                                                (t (takeMove from n (cdr graph)) )))


(defun grabMove(cvor n)(append (cons (car cvor) (list (removeN (cadr cvor) n )) ) (list (caddr cvor)) ))


(defun insertMove(cvor move)(append (cons (car cvor) (list (append move (cadr cvor) )) ) (list (caddr cvor)) ))




(defun playAndRemoveMove( from to n graph move)(cond ((null graph) '())
    ((eq (caar graph) from)  (if (eq to -1) (cons  (grabMove (car graph) (- (length (cadar graph) ) n) ) (cdr graph) ) 
    (cons  (grabMove (car graph) (- (length (cadar graph) ) n) ) (playAndRemoveMove -1 to n (cdr graph) move) )))
    ( (eq (caar graph) to) (if (eq from -1) (cons (insertMove (car graph) move)  (cdr graph) )
    (cons (insertMove (car graph) move)  (playAndRemoveMove from -1 n (cdr graph) move) ) ))
    (t (cons (car graph) (playAndRemoveMove from to n (cdr graph) move) ))))



(defun dots(n)(cond ((eq n 0) '() )
(t (cons '- (dots (1- n) ) ) )) )

(defun castBytes(li)(append  (dots (- 9 (1- (length li)) )) (reverse (cdr (reverse li)))))

(defun getThree(k li)(list (nth (* 3 (1- k )) li) (nth (1+ (* 3 (1- k ))) li) (nth (+ 2 (* 3 (1- k ))) li) ) )


(defun writeFirstLineOfNumbers(n)
  (cond
    ((> n sizeOfMatrix) '())
    (t (prog1
      (format t "      ~a" n)
      (writeFirstLineOfNumbers (1+ n))
      )
    )
  )
)

(defun drawTable(i j pom graph)(cond  ( (> i sizeOfMatrix) '())  ; mng bitna osmica
  ((and (eq i 1) (eq j 1) (eq pom 1)) 
  (prog1 (format t "~%   ")
  ;(format t "~%        1      2      3      4      5      6      7      8~%~%      ")
  (writeFirstLineOfNumbers 1)
  (format t "~%      ")
  (format t "~a" 
   (getThree pom ( castBytes (cadr (assoc (+ (* i 100) j) graph ) ) )))
    (drawTable i (1+ j) pom graph)) )
  ( (> j sizeOfMatrix) (if (eq pom 2)  (prog1 (format t "   ~a ~%      " (nth (1- i) karakteri)  ) (drawTable i 1 (1+ pom) graph))
   (if (eq pom 4) (prog1 (format t "~%     ") (drawTable (1+ i) 1 1 graph))  (prog1 (format t "~%      ") (drawTable i 1 (1+ pom) graph)) )))
  ( (eq pom 4) (prog1 (format t "") (drawTable (1+ i) 1 1 graph)))
  ( (not (eq (assoc (+ (* i 100) j) graph ) '())) (prog1 (format t "~a" 
   (getThree pom ( castBytes (cadr (assoc (+ (* i 100) j) graph ) ) ))) (drawTable i (1+ j) pom graph))  )
  (t (prog1 (format t "       "  )  (drawTable i (1+ j) pom graph)) )))

(defun shortestDistence(cvor graph )(min 
(distProp (cons (car cvor) obradjeni) (if (eq (contains obradjeni (nth 0 (last cvor))) t) nil (assoc (nth 0 (last cvor)) graph))graph)
(distProp (cons (car cvor) obradjeni) (if (eq (contains obradjeni (nth 1 (last cvor))) t) nil (assoc (nth 1 (last cvor)) graph))graph)
(distProp (cons (car cvor) obradjeni) (if (eq (contains obradjeni (nth 2 (last cvor))) t) nil (assoc (nth 2 (last cvor)) graph))graph)
(distProp (cons (car cvor) obradjeni) (if (eq (contains obradjeni (nth 3 (last cvor))) t) nil (assoc (nth 3 (last cvor)) graph))graph)
))

(defun distProp(obradjeni cvor  n graphGlobal)(cond ((null cvor) '())
  ((not (eq (caadr cvor ) 'E)) (list (list n (car cvor))) )
  (t (cons (list n (car cvor) 'E) (propagate obradjeni cvor  (1+ n) graphGlobal)))))

(defun propagate(obradjeni cvor n graphGlobal)(cond 
((eq n 1) (append 
(append
(list (distProp (cons (car cvor) obradjeni) (if (eq (contains obradjeni (nth 0 (car (last cvor)))) t) nil (assoc (nth 0 (car (last cvor))) graphGlobal)) n graphGlobal))
(list (distProp (cons (car cvor) obradjeni) (if (eq (contains obradjeni (nth 1 (car (last cvor)))) t) nil (assoc (nth 1 (car (last cvor))) graphGlobal)) n graphGlobal )))
(append 
(list (distProp (cons (car cvor) obradjeni) (if (eq (contains obradjeni (nth 2 (car (last cvor)))) t) nil (assoc (nth 2 (car (last cvor))) graphGlobal)) n graphGlobal))
(list (distProp (cons (car cvor) obradjeni) (if (eq (contains obradjeni (nth 3 (car (last cvor)))) t) nil (assoc (nth 3 (car (last cvor))) graphGlobal)) n graphGlobal)))
 ))
(t (append 
(append
(distProp (cons (car cvor) obradjeni) (if (eq (contains obradjeni (nth 0 (car (last cvor)))) t) nil (assoc (nth 0 (car (last cvor))) graphGlobal)) n graphGlobal)
(distProp (cons (car cvor) obradjeni) (if (eq (contains obradjeni (nth 1 (car (last cvor)))) t) nil (assoc (nth 1 (car (last cvor))) graphGlobal)) n graphGlobal))
(append 
(distProp (cons (car cvor) obradjeni) (if (eq (contains obradjeni (nth 2 (car (last cvor)))) t) nil (assoc (nth 2 (car (last cvor))) graphGlobal)) n graphGlobal)
(distProp (cons (car cvor) obradjeni) (if (eq (contains obradjeni (nth 3 (car (last cvor)))) t) nil (assoc (nth 3 (car (last cvor))) graphGlobal)) n graphGlobal))
)
 )))

(defun filterPaths(paths distance)(cond
  ((null paths) '())
  ((<= (caar (last (car paths))) distance) (cons (cadaar paths) (filterPaths (cdr paths) distance)))
  (t (filterPaths (cdr paths) distance))
))


(defun distOneCheck(li)(cond 
((null li)  '())
( (and (eq (length (car li)) 1) (not (equal (last (caar li)) (list 'E) ))) 
(cons (cadaar li) (distOneCheck (cdr li))))
(t  (distOneCheck (cdr li)) )))

(defun closestNodes(li)(cond 
((null li) 1000)
(t (min  (caar (last (car li)))  (closestNodes (cdr li) )) )) )


(defun height(li)(1- (length li)))


(defun allPossiblePlays(start paths heights graphGlobal)(cond 
  ((null paths) '())
  (t (append (possiblePlay start (car paths) heights graphGlobal)
  (allPossiblePlays start (cdr paths) heights graphGlobal)))
  ))
(defun possiblePlay(start path heights graphGlobal)
  (cond
    ((null heights) '())
    ((not (contains (cadr (assoc start graphGlobal)) whosPlaying)) '())
    (t  (cons (checkMove  start  path (car heights) graphGlobal) (possiblePlay start path (cdr heights) graphGlobal ) )) 
  ) 
)

(defun possibleHeights(stack n) 
  (cond
    ((null stack) '())
    ((equal whosPlaying (car stack)) (cons n (possibleHeights (cdr stack) (+ n 1))))
    (t (possibleHeights (cdr stack) (+ n 1 )))
  )
)

(defun checkMove(from to n graphGlobal)
  (cond
   ((eq (length (cadr (assoc to graphGlobal))) 1) (list from to n))
    ((> n (1- (length (cadr (assoc to graphGlobal))))) '())
    ( (> (+ (length (grabN (cadr (assoc from graphGlobal)) n)) (1- (length (cadr (assoc to graphGlobal))))) 8) '())
    (t (list from to n) )
  )
)

;(defun getLinks(cvor)(caddr cvor))
(defun contains (l el)
 (cond ((null l) '())
 ((equal el (car l)) t)
 (t (contains (cdr l) el))))


(defun moveGen(graph graphGlobal)(cond
  ((null graph) '())
  ((eq (length (cadr (assoc (caar graph) graphGlobal))) 1 ) (moveGen (cdr graph) graphGlobal))
  (t 
(let* ( ( allPaths  (propagate '() (assoc (caar graph) graphGlobal) 1 graphGlobal) )
( putevi (distOneCheck allPaths)) 
)(progn 
;; (print allPaths)
;; (print putevi)
(if (eq putevi '()) (setq putevi (filterPaths (removeNils allPaths) (closestNodes (removeNils allPaths)))) )
(append 
(progn
(allPossiblePlays (caar graph) putevi (possibleHeights
(cdr (reverse (cadr (assoc (caar graph) graphGlobal) ))) '1) graphGlobal)
)
(moveGen (cdr graph) graphGlobal)
))) )))

(setq graphGloball (makeGraph 8 9))


;(format t "Hello world.~%")
;(format t "Hello world.~%")
;(trace contains)
;(cons 'E putevi)


;; (setq put (propagate '() (assoc 707 graphGloball) 1 graphGloball  ))
;; (print put)
;; (print (distOneCheck put))
;(print (remove-duplicates (filterPaths putevi d) ))


;(format t "karakter ~a , karakter ~a" (nth '1 karakteri) (nth '0 karakteri)  )


;(trace filterPaths)
;(drawTable 1 1 1 (playMove 66 57 1(playMove 64 75 1 (makeGraph 8 9))))
;(setq d (propagate '() (assoc 11 graphGlobal) 1))
;(setq putevi (filterPaths putevi d))
;(setq putevi (remove-duplicates putevi))
;(print (possibleHeights (cdr (reverse (cadr (assoc 11 graphGlobal)))) '1))
;(print (allPossiblePlays 11 putevi (possibleHeights (cdr (reverse (cadr (assoc 11 graphGlobal)))) '1)))

(print (moveGen graphGloball graphGloball))
;(print (grabN (cdr (reverse (cadr (assoc 57 graphGlobal)))) '3))

;(print  graphGlobal)
(drawTable 1 1 1 graphGloball)
;(gameSetup)


(defun removeNils(li)(cond 
((null li) '())
( (equal (car li) nil) (removeNils (cdr li)))
(t (cons (car li) (removeNils (cdr li))))))

(defun validateMove (from to Depth graphGlobal)
    (let ((test (list-find (append (convert-me from) (convert-me to) (list (+ Depth 1))) (removeNils (moveGen graphGlobal graphGlobal)))))
      (cond ((null test) nil)
            (t test))
    ) 
)
;;(defun proveri(from)
;;    (if (>(length (cadr (assoc from graphGlobal))) 9)
;;          nil
          t
;;    ))

;;(setq graphGlobal (makeGraph 8 (1+ 8)))
;(print (playAndRemoveMove 503 -1 1 graphGlobal '(E)))
;(print graphGlobal)
;(print (cadr (assoc 303 graphGlobal)))

(defun validateAndPlayMove(from to Depth graphGlobal)
        (if (validateMove from to Depth graphGlobal)
          (let ((newGraph (playMove (car (convert-me from)) (car (convert-me to)) (+ Depth 1) graphGlobal) ))

            (if (eq (length (cadr (assoc (car (convert-me to)) newGraph))) 9)
                        (progn (setq finalStack (append finalStack (list (car (cadr (assoc (car (convert-me to)) newGraph)))))) 
                        (playAndRemoveMove (car (convert-me to)) -1 1 newGraph '(E)))
                        newGraph)
          )
          (prog1 graphGlobal (format t "~%~% Your move is invalid! ~%~%") (setq whosPlaying (if (eq whosPlaying 'X) 'O 'X)))
        )
        ;; ;(print (eq (length (cadr (assoc (car (convert-me to)) graphGlobal))) 3))
        
        ;;(print graphGlobal)
        ;;(print finalStack)
)
;;(playMove (convert-me from) (convert-me to) (+ Depth 1) graphGlobal)
(defun list-find (el lista)
    (cond ((null lista) '())
    ((equal el (car lista))(car lista))
    (t (list-find el (cdr lista)))
    ))


(defun convert-me (lista)
  (list (+ (* (+ (nth-elt (car lista) karakteri) 1) 100) (cadr lista)))
)

(defun nth-elt (elt list)
  (let ((loc (length (member elt list))))
    (unless (zerop loc)
      (- (length list) loc))))

;;(print  (convert-me '(B 22)))
;;(print (validatemove '(C 3) '(B 2) '0))
;;(validateAndPlayMove '(B 2) '(C 3) '0)
;;(print (list-find '(0 3 6) '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 3 7) (2 4 8) (0 4 8) (2 4 6))))
;;(print (uporedi '(0 1 2) '(0 1 2)))

(defun countStack(el currStack)
  (cond
    ((null currStack) 0)
    ((equal el (car currStack)) (+ 1 (countStack el (cdr currStack))))
    (t (countStack el (cdr currStack)))
  )
)

(defun checkFinalStack()
  (cond
    ((equal sizeOfMatrix 8) 
      (cond 
        ((equal (countStack 'X finalStack) 2) (append (list 'X) victor))
        ((equal (countStack 'O finalStack) 2) (append (list 'O) victor))
        (t '())
      )
    )
    ((equal sizeOfMatrix 10) 
      (cond
        ((equal (countStack 'X finalStack) 3) (append (list 'X) victor))
        ((equal (countStack 'O finalStack) 3) (append (list 'O) victor))
        (t '())
      )
    )
    (t '())
  )
)

(defun gameOver()
  (progn
    (format t "~%~% GAME OVER! ~% ~a HAS WON THE GAME!" victor)
  )
)

;; (defun checkEntry()
;;   (cond
;;     ((not (equal (length currentPlay) 3))
;;       (prog1 (format t "~%~% The move you have entered is invalid! ~%~%") (setq whosPlaying (if (eq whosPlaying 'X) 'O 'X))))
;;     (t )
;;   )
;; )

(defun weArePlaying (graphGlobal)
  (progn
    (format t "~% ~a is playing now! ~%" whosPlaying)
    (format t "~% Enter your move! ~%")
    (setq currentPlay (read ))
    (setq fromPlay (car currentPlay))
    (setq toPlay (cadr currentPlay))
    (setq heightPlay (caddr currentPlay))
    (let (( newStare (validateAndPlayMove fromPlay toPlay heightPlay graphGlobal)))
    (progn
    (drawTable 1 1 1 newStare)
    (format t "~%  ~%")
    (setq whosPlaying (if (eq whosPlaying 'X) 'O 'X))
    (cond 
      ((checkFinalStack) (gameOver))
      (t (weArePlaying newStare))
    )
    (weArePlaying newStare)
  ))
)
)

(defun gameSetup()  
  (progn 
    (progn (format t "~% Enter who is playing first: X for black, or, O for white! ~%")
            (setq whosPlayingFirst (read ))
            (cond 
              ((equalp whosPlayingFirst 'X) (format t "~% Black (X) is playing first! ~%"))
              ((equalp whosPlayingFirst 'O) (format t "~% White (O) is playing first! ~%"))
              (t (progn (format t "~% You have not entered a valid character! ~%") (gameSetup )))
            )
            (setq whosPlaying whosPlayingFirst)
    )
    (progn (format t "~% Enter what color do you want to be: X for black, or, O for white! ~%")
            (setq whoAmI (read ))
            (cond 
              ((equalp whoAmI 'X) (progn (format t "~% Your are black (X) and yout opponent is white (O)! ~%") (setq whoIsAI 'O)))
              ((equalp whoAmI 'O) (progn (format t "~% Your are white (O) and yout opponent is black (X)! ~%") (setq whoIsAI 'X)))
              (t (progn (format t "~% You have not entered a valid character! ~%") (gameSetup )))
            )
    )
    (progn (format t "~% Enter the size of the matrix! ~%")
            (setq sizeOfMatrix (read ))
            (cond
              ((< sizeOfMatrix 8) (progn (format t "~% You have not entered a valid number! ~%") (gameSetup)))
              ((> sizeOfMatrix 26) (progn (format t "~% You have not entered a valid number! ~%") (gameSetup)))
              ((not (equal (mod sizeOfMatrix 2 ) 0)) (progn (format t "~% You have not entered a valid number! ~%") (gameSetup)))
              ((equal (mod sizeOfMatrix 2) 0) (progn (format t "~% The size of your matrix is ~a ! ~%" sizeOfMatrix) ))
              (t (gameSetup))
            )
    )
    (let (( graphGlobal (makeGraph sizeOfMatrix (1+ sizeOfMatrix))) )
    (progn 
    (drawTable 1 1 1 graphGlobal)
    (weArePlaying graphGlobal)
    )
    )
  )
)



(defun genStates(allMoves graphGlobal)(cond 
((null allMoves)) '() 
(t (append (playMove (nth 0 (car allMoves)) (nth 1 (car allMoves)) (nth 2 (car allMoves)) graphGlobal)   (genStates (cdr allMoves) graphGlobal)  ))))

;(gameSetup)

