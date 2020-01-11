(defvar karakteri (list 'A 'B 'C 'D 'E 'F 'G 'H 'I 'J 'K 'L 'M 'N 'O 'P 'Q 'R 'S 'T 'U 'V 'W 'X 'Y 'Z))
(defvar putevi '())

(defvar sizeOfMatrix '8)

(defvar whosPlaying 'O)
(defvar whosPlayingFirst 'X)
(defvar whoIsAI 'O)
(defvar whoAmI 'X)

(defvar finalStack '())
(defvar victor '())
(setf *random-state* (make-random-state t))

(defun makeGraph(m n)
  (cond 
    ((< (+ (* m m) m) n) (format t "~%") )
    ((<= (floor (/ n m)) 1 ) (cons (makeCvor (1- n) m t)   (makeGraph m (+ n 2)) ))
    ((>= (floor (/ n m)) m) (cons (makeCvor n m t)  (makeGraph m (+ n 2)) ))
    (( eq (1+ (mod n m )) m) (cons (makeCvor n m nil) (makeGraph m (+ n 1))))
    (( eq (1+ (mod n m)) (- m 1)) (cons (makeCvor n m nil) (makeGraph m (+ n 3))))
    (t (cons (makeCvor n m nil) (makeGraph m (+ n 2))))
  )
)

(defun umetni(n m)
  (list 
    (if (>= (1- (floor (/ n m))) 1) (if (< (1- (1+ (mod n m))) 1) 0 ( + (* (1- (floor (/ n m))) 100) (1- (1+ (mod n m))) )) 0)
    (if (>= (1- (floor (/ n m))) 1) (if (> (1+ (1+ (mod n m))) m) 0 ( + (* (1- (floor (/ n m))) 100) (1+ (1+ (mod n m))) )) 0)
    (if (<= (1+ (floor (/ n m))) m) (if (> (1+ (1+ (mod n m))) m) 0 ( + (* (1+ (floor (/ n m))) 100) (1+ (1+ (mod n m))) )) 0)
    (if (<= (1+ (floor (/ n m))) m) (if (< (1- (1+ (mod n m))) 1) 0 ( + (* (1+ (floor (/ n m))) 100) (1- (1+ (mod n m))) )) 0)
  )
)

(defun makeCvor(n m empty)
  (cons (+ (* 100 (floor (/ n m))) (1+ (mod n m)))  (if (eq empty t)   (cons (list 'E)  (list (umetni n m)))  (cons  (if (eq (1+ (mod (floor (/ n m)) 2)) 1 ) (list 'X 'E) (list 'O 'E)) 
  (list(umetni n m)) ))) 
)

(defun playMove(from to n graph)
  (playAndRemoveMove from to n graph (takeMove from n graph))
)

(defun grabN(li n)
  (cond ((null li) '())
    ((eq n 0) '())
    (t (cons (car li) (grabN (cdr li) (1- n))) )
  )
)

(defun removeN(li n)
  (cond ((null li) '())
    ((eq n 0) li)
    (t (removeN (cdr li) (1- n)) )
  )
)

(defun takeMove(from n graph)
  (cond
    ((null graph) '())
    ((eq (caar graph) from) (grabN (cadar graph) (- (length (cadar graph) ) n )))
    (t (takeMove from n (cdr graph)) )
  )
)

(defun grabMove(cvor n)
  (append 
    (cons (car cvor) (list (removeN (cadr cvor) n )) ) (list (caddr cvor)) 
  )
)

(defun insertMove(cvor move)
  (append 
    (cons (car cvor) (list (append move (cadr cvor) )) ) (list (caddr cvor)) 
  )
)

(defun playAndRemoveMove( from to n graph move)
  (cond 
    ((null graph) '())
    ((eq (caar graph) from)  (if (eq to -1) (cons  (grabMove (car graph) (- (length (cadar graph) ) n) ) (cdr graph) ) 
      (cons  (grabMove (car graph) (- (length (cadar graph) ) n) ) (playAndRemoveMove -1 to n (cdr graph) move) )))
    ((eq (caar graph) to) (if (eq from -1) (cons (insertMove (car graph) move)  (cdr graph) )
      (cons (insertMove (car graph) move)  (playAndRemoveMove from -1 n (cdr graph) move) ) ))
    (t (cons (car graph) (playAndRemoveMove from to n (cdr graph) move) ))
  )
)

(defun dots(n)
  (cond 
    ((eq n 0) '() )
    (t (cons '- (dots (1- n))))
  )
)

(defun castBytes(li)
  (append 
    (dots (- 9 (1- (length li)) )) (reverse (cdr (reverse li)))
  )
)

(defun getThree(k li)
  (list (nth (* 3 (1- k )) li) (nth (1+ (* 3 (1- k ))) li) (nth (+ 2 (* 3 (1- k ))) li))
)

(defun writeFirstLineOfNumbers(n)
  (cond
    ((> n sizeOfMatrix) '())
    (t (prog1
      (format t "      ~a" n)
      (writeFirstLineOfNumbers (1+ n))
    ))
  )
)

(defun drawTable(i j pom graph)
  (cond 
    ((> i sizeOfMatrix) '())
    ((and (eq i 1) (eq j 1) (eq pom 1)) 
      (prog1 (format t "~%   ")
        (writeFirstLineOfNumbers 1)
        (format t "~%      ")
        (format t "~a" (getThree pom ( castBytes (cadr (assoc (+ (* i 100) j) graph ) ) )))
        (drawTable i (1+ j) pom graph)))
    ((> j sizeOfMatrix) 
      (if (eq pom 2) 
        (prog1 (format t "   ~a ~%      " (nth (1- i) karakteri)  ) (drawTable i 1 (1+ pom) graph))
        (if (eq pom 4) (prog1 (format t "~%     ") (drawTable (1+ i) 1 1 graph))  (prog1 (format t "~%      ") (drawTable i 1 (1+ pom) graph)) )))
    ((eq pom 4) (prog1 (format t "") (drawTable (1+ i) 1 1 graph)))
    ((not (eq (assoc (+ (* i 100) j) graph ) '())) 
      (prog1 (format t "~a" (getThree pom ( castBytes (cadr (assoc (+ (* i 100) j) graph ) ) ))) 
             (drawTable i (1+ j) pom graph)))
    (t (prog1 (format t "       "  ) 
              (drawTable i (1+ j) pom graph)))
  )
)

(defun distProp(obradjeni cvor  n graphGlobal)
  (cond 
    ((null cvor) (list (list '100 'E 'E)))
    ((eq cvor 0) '())
    ((not (eq (caadr cvor ) 'E)) (list (list n (car cvor))) )
    (t (cons (list n (car cvor) 'E) (propagate obradjeni cvor  (1+ n) graphGlobal)))
  )
)

(defun propagate(obradjeni cvor n graphGlobal)
  (cond 
    ((eq n 1)
      (append 
        (append
          (list (distProp (append (caddr cvor) obradjeni) (if (eq (contains obradjeni (nth 0 (car (last cvor)))) t) 0 (assoc (nth 0 (car (last cvor))) graphGlobal)) n graphGlobal))
          (list (distProp (append (caddr cvor)  obradjeni) (if (eq (contains obradjeni (nth 1 (car (last cvor)))) t) 0 (assoc (nth 1 (car (last cvor))) graphGlobal)) n graphGlobal ))
        )
        (append 
          (list (distProp (append (caddr cvor)  obradjeni) (if (eq (contains obradjeni (nth 2 (car (last cvor)))) t) 0 (assoc (nth 2 (car (last cvor))) graphGlobal)) n graphGlobal))
          (list (distProp (append (caddr cvor)  obradjeni) (if (eq (contains obradjeni (nth 3 (car (last cvor)))) t) 0 (assoc (nth 3 (car (last cvor))) graphGlobal)) n graphGlobal))
        )
      ))
    (t 
      (append 
        (append
          (distProp (append (caddr cvor)  obradjeni) (if (eq (contains obradjeni (nth 0 (car (last cvor)))) t) 0 (assoc (nth 0 (car (last cvor))) graphGlobal)) n graphGlobal)
          (distProp (append (caddr cvor)  obradjeni) (if (eq (contains obradjeni (nth 1 (car (last cvor)))) t) 0 (assoc (nth 1 (car (last cvor))) graphGlobal)) n graphGlobal)
        )
        (append 
          (distProp (append (caddr cvor)  obradjeni) (if (eq (contains obradjeni (nth 2 (car (last cvor)))) t) 0 (assoc (nth 2 (car (last cvor))) graphGlobal)) n graphGlobal)
          (distProp (append (caddr cvor)  obradjeni) (if (eq (contains obradjeni (nth 3 (car (last cvor)))) t) 0 (assoc (nth 3 (car (last cvor))) graphGlobal)) n graphGlobal)
        )
      ))
  )
)

(defun filterPaths(paths distance ret)
  (cond
    ((null paths) '())
    ((not (atom (caar paths)) ) (append (filterPaths (car paths) distance (caar paths)) (filterPaths (cdr paths) distance '()) ))
    ((eq (length (car paths)) 3) (filterPaths (cdr paths) distance ret))
    ((<= (caar paths) distance) (list (cadr ret)))
    (t (filterPaths (cdr paths) distance ret))
  )
)


(defun distOneCheck(li)
  (cond 
    ((null li)  '())
    ((and (eq (length (car li)) 1) (not (equal (last (caar li)) (list 'E) ))) 
      (cons (cadaar li) (distOneCheck (cdr li))))
    (t (distOneCheck (cdr li)) )
  )
)

(defun closestNodes(li)
  (cond 
    ((null li) 1000)
    ((not (atom (caar li)) ) (min (closestNodes (car li)) (closestNodes (cdr li)) ))
    ((eq (length (car li)) 3) (closestNodes (cdr li)))
    (t (min  (caar li)  (closestNodes (cdr li))))
  )
)

(defun height(li)(1- (length li)))

(defun allPossiblePlays(start paths heights graphGlobal)
  (cond 
    ((null paths) '())
    (t (append (possiblePlay start (car paths) heights graphGlobal)
      (allPossiblePlays start (cdr paths) heights graphGlobal)))
  )
)

(defun possiblePlay(start path heights graphGlobal)
  (cond
    ((null heights) '())
    ((not (contains (cadr (assoc start graphGlobal)) whosPlaying)) '())
    (t  (append (checkMove  start  path (car heights) graphGlobal) (possiblePlay start path (cdr heights) graphGlobal ) )) 
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
   ( (> (+ (length (removeN (cadr (assoc from graphGlobal)) (1- n) )) (1- (length (cadr (assoc to graphGlobal))))) 9) '())
   ((eq n 1) (list (list from to n)))
   ((> n (1- (length (cadr (assoc to graphGlobal))) )) '())
   (t (list (list from to n)) )
  )
)

(defun contains (l el)
  (cond 
    ((null l) '())
    ((equal el (car l)) t)
    (t (contains (cdr l) el))
  )
)

(defun moveGen(graph graphGlobal)
  (cond
    ((null graph) '())
    ((eq (length (cadr (assoc (caar graph) graphGlobal))) 1 ) (moveGen (cdr graph) graphGlobal))
    (t (let* (( allPaths  (propagate (list (caar graph)) (assoc (caar graph) graphGlobal) 1 graphGlobal))( putevi (distOneCheck allPaths)))
              (progn 
                (if (eq putevi '()) 
                  (prog1 
                    (setq putevi (filterPaths (removeNils allPaths) (closestNodes (removeNils allPaths)) '()))))
                (append (progn (allPossiblePlays (caar graph) putevi (possibleHeights (cdr (reverse (cadr (assoc (caar graph) graphGlobal) ))) '1) graphGlobal)) (moveGen (cdr graph) graphGlobal)))))
  )
)

(defun removeNils(li)
  (cond 
    ((null li) '())
    ((equal (car li) nil) (removeNils (cdr li)))
    (t (cons (car li) (removeNils (cdr li))))
  )
)

(defun validateMove (from to Depth graphGlobal)
    (let ((test (list-find (append (convert-me from) (convert-me to) (list (+ Depth 1))) (removeNils (moveGen graphGlobal graphGlobal)))))
      (cond ((null test) nil)
            (t test))
    ) 
)

(defun validateAndPlayMove(from to Depth graphGlobal)
  (cond 
    ((equal whosPlaying whoAmI)
      (if (validateMove from to Depth graphGlobal)
        (let ((newGraph (playMove (car (convert-me from)) (car (convert-me to)) (+ Depth 1) graphGlobal) ))
          (if (eq (length (cadr (assoc (car (convert-me to)) newGraph))) 9)
              (progn (setq finalStack (append finalStack (list (car (cadr (assoc (car (convert-me to)) newGraph)))))) 
                      (playAndRemoveMove (car (convert-me to)) -1 1 newGraph '(E))) newGraph))
          (prog1 graphGlobal (format t "~%~% Your move is invalid! ~%~%") (setq whosPlaying (if (eq whosPlaying 'X) 'O 'X)))
      )
    )
    (t 
      (let ((newGraph (playMove (car (convert-me from)) (car (convert-me to)) (+ Depth 1) graphGlobal) ))
        (if (eq (length (cadr (assoc (car (convert-me to)) newGraph))) 9)
            (progn (setq finalStack (append finalStack (list (car (cadr (assoc (car (convert-me to)) newGraph)))))) 
                    (playAndRemoveMove (car (convert-me to)) -1 1 newGraph '(E))) newGraph))
        (prog1 graphGlobal (format t "~%~% Your move is invalid! ~%~%") (setq whosPlaying (if (eq whosPlaying 'X) 'O 'X)))
    )
  )
)

(defun list-find (el lista)
  (cond
    ((null lista) '())
    ((equal el (car lista))(car lista))
    (t (list-find el (cdr lista)))
  )
)

(defun convert-me (lista)
  (list (+ (* (+ (nth-elt (car lista) karakteri) 1) 100) (cadr lista)))
)

(defun nth-elt (elt list)
  (let ((loc (length (member elt list))))
        (unless (zerop loc) (- (length list) loc))
  )
)

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
        ((equal (countStack 'X finalStack) 2) 
          (progn 
            (setq victor (append (list 'X) victor))
            t))
        ((equal (countStack 'O finalStack) 2) 
          (progn
            (setq victor (append (list 'O) victor))
            t))
        (t '())
      )
    )
    ((equal sizeOfMatrix 10) 
      (cond
        ((equal (countStack 'X finalStack) 3) 
          (progn
            (setq victor (append (list 'X) victor))
            t))
        ((equal (countStack 'O finalStack) 3) 
          (progn
            (setq victor (append (list 'O) victor))
            t))
        (t '())
      )
    )
    (t '())
  )
)

(defun weArePlaying (graphGlobal)
  (cond 
    ((equal whosPlaying whoAmI) 
      (progn
        (format t "~% ~a is playing now! ~%" whosPlaying)
        (format t "~% Enter your move! ~%")
        (setq currentPlay (read ))
        (cond 
          ((equal currentPlay '()) (progn
              (drawTable 1 1 1 graphGlobal)
              (format t "~% You just played a NULL Move! ~%")
              (setq whosPlaying (if (eq whosPlaying 'X) 'O 'X))
              (weArePlaying graphGlobal)
            ))
          (t (progn
                (setq fromPlay (car currentPlay))
                (setq toPlay (cadr currentPlay))
                (setq heightPlay (caddr currentPlay))
                (let (( newState (validateAndPlayMove fromPlay toPlay heightPlay graphGlobal)))
                  (progn
                    (drawTable 1 1 1 newState)
                    (format t "~%  ~%")
                    (setq whosPlaying (if (eq whosPlaying 'X) 'O 'X))
                    (cond 
                      ((checkFinalStack) (format t "~%~% GAME OVER! ~% ~a HAS WON THE GAME!" victor))
                      (t (weArePlaying newState)))))
            ))
        )
      )
    )
    (t
      ;; (equal whosPlaying whoIsAI) 
      (progn
        (format t "~% ~a is playing now! ~%" whosPlaying)
        (format t "~% AI is on the move! ~%")
        (setq alphBetEnd (alphaBeta graphGlobal '() 3 -10000 10000 t))
        (cond
          ((equal (car alphBetEnd) -100000) (progn
              (drawTable 1 1 1 graphGlobal)
              (format t "~% AI just played a NULL Move! ~%")
              (setq whosPlaying (if (eq whosPlaying 'X) 'O 'X))
              (weArePlaying graphGlobal)
            ))
          (t (progn
            (setq currentPlay (cadr alphBetEnd))
            (print currentPlay)
            (setq fromPlay (car currentPlay))
            (setq toPlay (cadr currentPlay))
            (setq heightPlay (caddr currentPlay))
            (let (( newState (playMove fromPlay toPlay heightPlay graphGlobal)))
                (progn
                  (drawTable 1 1 1 newState)
                  (format t "~%  ~%")
                  (setq whosPlaying (if (eq whosPlaying 'X) 'O 'X))
                  (cond 
                    ((checkFinalStack) (format t "~%~% GAME OVER! ~% ~a HAS WON THE GAME!" victor))
                    (t (weArePlaying newState)))))
            ))
        )
      )
    )
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

(defun genStates(allMoves graphGlobal playing)
  (cond 
    ((null allMoves) '() )
    (t (progn (setq whosPlaying playing)
    (cons (playMove (nth 0 (car allMoves)) (nth 1 (car allMoves)) (nth 2 (car allMoves)) graphGlobal) (genStates (cdr allMoves) graphGlobal playing))))
  )
)

(defun alphaBeta(state move depth alpha beta maxPlayerr)
  (cond
    ((eq depth 0) (list (heuristika state maxPlayerr) move))
    ((eq maxPlayerr t)
       (maxPlayer (genStates (moveGen state state) state whoIsAI) (moveGen state state) depth alpha beta))
    (t (minPlayer (genStates (moveGen state state) state whoAmI) (moveGen state state) depth alpha beta))
  )
)

(defun minPlayer(states moves depth alpha beta)
  (whileMin (list 100000) states moves depth alpha beta t)
)

(defun maxPlayer(states moves depth alpha beta)
  (whileMax (list -100000) states moves depth alpha beta t)
)

(defun whileMin(evalMin states moves depth alpha beta doWhile)
  (cond
    ((eq doWhile nil) evalMin)
    ((null states) evalMin)
    (t (let ((eval (alphaBeta (car states) (car moves) (1- depth) alpha beta t)))
            (whileMin (list (min (car evalMin) (car eval)) (if (eq (min (car evalMin) (car eval)) (car eval)) (car moves) (cadr evalMin))) 
            (cdr states) (cdr moves) depth  alpha  (min beta (car eval)) (if (<= (min beta (car eval))  alpha) nil t))))
  )
)

(defun whileMax(evalMax states moves depth alpha beta doWhile )
  (cond
    ((eq doWhile nil) evalMax)
    ((null states) evalMax)
    (t (let ((eval (alphaBeta (car states) (car moves) (1- depth) alpha beta nil)))
            (whileMax (list (max (car evalMax) (car eval)) (if (eq (max (car evalMax) (car eval)) (car eval)) (car moves) (cadr evalMax))) 
            (cdr states) (cdr moves) depth (max alpha (car eval)) beta (if (<= beta (max alpha (car eval))) nil t))))
  )
)

(defun countTops(state count)
  (cond
    ((null state) count) 
    ((eq (caadar state) whoIsAI) (countTops (cdr state) (+ (- (length (cadar state)) 2) count))) 
    (t (countTops (cdr state) count))
  )
)

(defun checkEightStack(state)
  (cond
    ((null state) 0)
    ((eq (length (cadar state)) 9) 20)
    (t (checkEightStack (cdr state)) )
  )
)

(defun checkAdjisentStacks(state count graphGlobal)
  (cond
    ((null state) count)
    (t (checkAdjisentStacks (cdr state) (checkAdjisent (caddar state) 0 graphGlobal) graphGlobal))
  )
)


(defun checkAdjisent(links count graph)
  (cond
    ((null links) count)
    ((eq (caadr (assoc (car links) graph)) whoIsAI) (checkAdjisent (cdr links) (+ 2 count) graph))
    (t (checkAdjisent (cdr links) count graph))
  )
)


(defun heuristika(state player)
  (+ (+ (countTops state 0) (checkEightStack state)) (if (eq player nil) (checkAdjisentStacks state 0 state) 0))
)



(gameSetup)

;;(defvar graphGloball (makeGraph 8 9))
;;(defvar test '((101 (E) (0 0 202 0)) (103 (E) (0 0 204 202)) (105 (E) (0 0 206 204)) (107 (E) (0 0 208 206)) (202 (E) (101 103 303 301))
;;  (204 (E) (103 105 305 303)) (206 (E) (105 107 307 305)) (208 (E) (107 0 0 307)) (301 (E) (0 202 402 0)) (303 (E) (202 204 404 402))
;;  (305 (O  E) (204 206 406 404)) (307 (E) (206 208 408 406)) (402 (E) (301 303 503 501)) (404 (E) (303 305 505 503))
;;  (406 (X X X E) (305 307 507 505)) (408 (E) (307 0 0 507)) (501 (E) (0 402 602 0)) (503 (E) (402 404 604 602)) (505 (E) (404 406 606 604))
;;  (507 (O X X X E) (406 408 608 606)) (602 (E) (501 503 703 701)) (604 (E) (503 505 705 703)) (606 (E) (505 507 707 705))
;;  (608 (E) (507 0 0 707)) (701 (E) (0 602 802 0)) (703 (E) (602 604 804 802)) (705 (E) (604 606 806 804)) (707 (E) (606 608 808 806))
;;  (802 (E) (701 703 0 0)) (804 (E) (703 705 0 0)) (806 (E) (705 707 0 0)) (808 (E) (707 0 0 0))))
;;  (print (caadr (assoc 202 test)))
;;  (print (heuristika test nil))
;; (drawTable 1 1 1 test)

;; (defvar graphGloball (makeGraph 8 9))
;; (defvar test '((101 (E) (0 0 202 0)) (103 (E) (0 0 204 202)) (105 (E) (0 0 206 204)) (107 (E) (0 0 208 206)) (202 (E) (101 103 303 301))
;;  (204 (E) (103 105 305 303)) (206 (E) (105 107 307 305)) (208 (E) (107 0 0 307)) (301 (E) (0 202 402 0)) (303 (E) (202 204 404 402))
;;  (305 (E) (204 206 406 404)) (307 (E) (206 208 408 406)) (402 (E) (301 303 503 501)) (404 (E) (303 305 505 503))
;;  (406 (E) (305 307 507 505)) (408 (E) (307 0 0 507)) (501 (E) (0 402 602 0)) (503 (E) (402 404 604 602)) (505 (E) (404 406 606 604))
;;  (507 (E) (406 408 608 606)) (602 (E) (501 503 703 701)) (604 (E) (503 505 705 703)) (606 (E) (505 507 707 705))
;;  (608 (E) (507 0 0 707)) (701 (E) (0 602 802 0)) (703 (E) (602 604 804 802)) (705 (E) (604 606 806 804)) (707 (E) (606 608 808 806))
;;  (802 (E) (701 703 0 0)) (804 (E) (703 705 0 0)) (806 (E) (705 707 0 0)) (808 (E) (707 0 0 0))))
;; (drawTable 1 1 1 test)


;; (trace genStates)
;;(print (car (alphaBeta test '() 2 -10000 10000 t)))
;; (print (playMove 303 202 1 graphGloball))
;; (print (moveGen test test))
;; (print (genStates (moveGen test test) test 'O))
;; (print (moveGen graphGloball graphGloball))