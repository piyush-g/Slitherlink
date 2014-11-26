(defparameter rows 5)
(defparameter colomns 5)
(defparameter board nil)
(defparameter symbol-horizontal 'H)
(defparameter symbol-vertical 'V)
(defparameter horizontal-edges nil)
(defparameter vertical-edges nil)
(defparameter filename nil)
(defparameter input 0)
(defparameter x 0)
(defparameter y 0)
(defparameter z 0)
(defparameter edge-count 0)
(defparameter temp 0)
(defparameter temph nil)
(defparameter tempv nil)
(defparameter existing-neighbors ())
(defparameter edge-count-in-loop 0)
(defparameter first-edge nil)

(defun construct-list(items value)
	(cond ((>= 1 items) (list value)) 
				( t (append (list value) (construct-list (1- items) value)))))

(defun construct-matrix(row colomn value)
	(cond ((>= 1 row) (list (construct-list colomn value)))
				(t (cons (construct-list colomn value) (construct-matrix (1- row) colomn value)))))

( defun element(board i j)
	(nth j (nth i board)))

(defun set-element(board i j value)
	(setf (nth j (nth i board)) value))

(defun get-neighbor-horizontal-edge(i j)
	 (list 	(list i (1- j) symbol-horizontal) (list i (1+ j) symbol-horizontal) 
	 				(list (1- i) j symbol-vertical) (list (1- i) (1+ j) symbol-vertical)
	 				(list i j symbol-vertical) (list i (1+ j) symbol-vertical)))

(defun get-neighbor-vertical-edge(i j)
	(list (list (1- i) j symbol-vertical) (list (1+ i) j symbol-vertical)
				(list i (1- j) symbol-horizontal) (list i j symbol-horizontal)
				(list (1+ i) (1- j) symbol-horizontal) (list (1+ i) j symbol-horizontal)))

(defun get-neighbors(edge)
	(if (equal (nth 2 edge) symbol-horizontal) (get-neighbor-horizontal-edge (nth 0 edge)(nth 1 edge))
																						(get-neighbor-vertical-edge (nth 0 edge)(nth 1 edge))	)
)

(defun is-edge-exists(edge)
	( if (>= (nth 0 edge) 0)
		( if (>= (nth 1 edge) 0)
			( if (equal 1 
							( if 	(equal symbol-horizontal (nth 2 edge))
										(element horizontal-edges (nth 0 edge)(nth 1 edge)) 
										(element vertical-edges (nth 0 edge)(nth 1 edge))
							)
						)
				;;----------------------
				T
				NIL
			)
		)
	)
)

(defun get-existing-edges(edge-list) 
	(setf existing-neighbors nil)
	(loop 
		for edge in edge-list do
		(if (equal T (is-edge-exists edge))
			(if (equal nil existing-neighbors)
				(setf existing-neighbors (list edge))
				(setf existing-neighbors (append existing-neighbors (list edge)))
			)
		)
	)
	existing-neighbors
)

(defun get-existing-neighbor-edges(edge)
		(get-existing-edges (get-neighbors edge))
) 
(defun get-next-edge-in-loop(neighbor-list prev-edge)
	(if (are-edges-equal prev-edge (car neighbor-list)) 
			(car (cdr neighbor-list)) 
			(if (are-edges-equal prev-edge (car(cdr neighbor-list))) (car neighbor-list))
	)
)

( defun read-file(filename)
	(let ((in (open filename :if-does-not-exist nil)))
	(setq	rows (read in))
	(setq	colomns (read in))
	(setq board (construct-matrix rows colomns 5))
	(setq horizontal-edges (construct-matrix (1+ rows) colomns 0))
	(setq vertical-edges (construct-matrix rows (1+ colomns) 0))
  (when in
    (loop for i = (read in nil)
    			for j = (read in nil)
    			for val = (read in nil)
    			while val  do (set-element board i j val))
    (close in))))

(defun print-horizontal-row(edge)
	(cond ((>=  0 (length edge)) (format t "+"))
				(t 
					(cond 	((= 1 (car edge)) (format t "+-") )
							(t  
								(format t "+ ")
								(print-horizontal-row(cdr edge))	
							)
						
				))))

;; helper function for finding first element in the matrix with value 1.
;; used to find the first edge 
(defun get-first-element-in-list(lst row colomn edge-type)
	(loop for i from 0 to (1- row) do
		(loop for j from 0 to (1- colomn) do
			(cond ((= 1 (element lst i j)) (return-from get-first-element-in-list (list i j edge-type)))))))

;;returns the first edge to start the check whether the puzzle got completed or not
(defun get-first-edge(horizontal-edges vertical-edges)
	(setq temph (get-first-element-in-list horizontal-edges (1+ rows) colomns 'h))
	(cond ((equal nil temph) 
				 	(setq tempv (get-first-element-in-list vertical-edges rows (1+ colomns) 'v))
				 	(cond ((equal nil tempv)  tempv)
				 				( t tempv)))
				( t temph)
))

(defun are-edges-equal(edge1 edge2)
	(cond (
					(equal (length edge1) (length edge2)) 
					(cond ((equal nil edge1) 
										(cond ((equal nil edge2) T)
											(T nil))
								)
								( T (and (equal (car edge1) (car edge2)) (are-edges-equal (cdr edge1) (cdr edge2))))
					)
				)
				( T nil)
	)) 
;; function to count the number of edges in the whole puzzle.
(defun count-edges(edge-list)
	(cond ((atom (car edge-list)) 
						(cond ((equal 1 (car edge-list)) (1+ (count-edges (cdr edge-list))))
									((equal nil (car edge-list)) 0)
									(t (count-edges (cdr edge-list)))))

				((listp (car edge-list)) (+ (count-edges (car edge-list)) (count-edges (cdr edge-list))))
	))

(defun print-horizontal-edge(edge)
	(loop for i in edge
		do (cond 	((= 1 i) (format t "+-"))
						( t (format t "+ "))))
	(format t "+")
)

(defun print-board-row(boardrow verticalrow colomns)
	(loop for i from 0 to (1- colomns)
				do(cond ((= 1 (nth i verticalrow)) (format t "|"))
								( t (format t " ")))
				do(cond ((= 5 (nth i boardrow)) (format t " "))
								( t (princ (nth i boardrow)))))
	(cond ((= 1 (nth colomns verticalrow)) (format t "|"))
								( t (format t " "))))

(defun make-board-row(board-row vertical-edge-row)
	(loop for i in board-row
				for j in vertical-edge-row
				collect (list i j)))

(defun update-horizontal-edge(x y z val)
	(set-element horizontal-edges (+ x z) y val))
(defun update-vertical-edge(x y z val)
	(set-element vertical-edges x (+ y z) val))

(defun update-edge(x y which-edge val)
	(cond ((equal 'T which-edge) (update-horizontal-edge x y 0 val)) ;; top-edge
				((equal 'B which-edge) (update-horizontal-edge x y 1 val)) ;; bottom-edge
				((equal 'L which-edge) (update-vertical-edge x y 0 val)) ;; left-edge
				((equal 'R which-edge) (update-vertical-edge x y 1 val)))) ;; right-edge

(defun get-coordinates(val) 
	(princ "Enter the edge for the element")
	(terpri)
	(setq x (read ))
	(setq y (read ))
	(setq z (read ))
	(if (or (> x rows) (> y colomns) (not (or (equal 'T z) (equal 'R z) (equal 'L z) (equal 'B z))))
		(progn 
			(princ "Invalid edge. Please try again: ")
			(terpri)
			(get-coordinates val)
		)
		(update-edge (1- x) (1- y) z val)
	)
)

(defun find-loop(prev-edge current-edge)
	(if (equal nil prev-edge) (return-from find-loop NIL))
	(if (equal nil current-edge) (return-from find-loop NIL))
	(let 	((next-edge nil)
				 (neighbor-list ())	
				)
			(setq neighbor-list (get-existing-neighbor-edges current-edge))
			(if (not (equal 2 (length neighbor-list))) (return-from find-loop NIL))
			(setf next-edge (get-next-edge-in-loop neighbor-list prev-edge))
			(if (equal next-edge nil) (return-from find-loop NIL))
			(setq edge-count-in-loop (1+ edge-count-in-loop))
			(if (equal next-edge first-edge) T (find-loop current-edge next-edge))
	)
)

(defun check-loop()
	(setf first-edge (get-first-edge horizontal-edges vertical-edges))
	(if (equal first-edge nil) (return-from check-loop NIL))
	(setf edge-count-in-loop 1)
	(find-loop first-edge (nth 0 (get-existing-neighbor-edges first-edge)))
)

(defun check-if-completed()
		(if (validate-each-board-entry)
			(if (check-loop) 
					( if (equal (+ (count-edges horizontal-edges)(count-edges vertical-edges)) edge-count-in-loop)
								T 
								NIL
					)
					NIL
			)
			NIL
		)
)
(defun validate-each-board-entry()
	(loop for i from 0 to (1- rows) do
		(loop for j from 0 to (1- colomns) do
				(setf temp (element board i j))
				(if (not (equal 5 temp))
					(if (not (equal temp (length (get-existing-edges (get-all-edges-for-board-entry i j)))))
						(return-from validate-each-board-entry NIL)
					)
				)
		)
	)
	T
)
(defun get-first-entry-in-matrix()
	(loop for i from 0 to (1- rows) do
		(loop for j from 0 to (1- colomns) do
			(if (not (equal 5 temp))
				(return-from get-first-entry-in-matrix (list i j))
			)
		)
	)
)

(defun get-all-edges-for-board-entry(i j)
	(list (list i j symbol-horizontal) (list (1+ i) j symbol-horizontal)
				(list i j symbol-vertical) (list i (1+ j) symbol-vertical)
	)
)

(defun print-board(board horizontal-edges vertical-edges rows colomns)
	(print-horizontal-edge (nth 0 horizontal-edges))
	(terpri)
	(loop for i from 1 to rows
	do (print-board-row (nth (1- i) board) (nth (1- i) vertical-edges) colomns)
	do (terpri)
	do (print-horizontal-edge (nth i horizontal-edges))
	do (terpri)
	))

(defun make-edge()
	(get-coordinates 1)
	(print-board board horizontal-edges vertical-edges rows colomns)
	(if (check-if-completed) (game-finished) (get-input))
)

(defun delete-edge()
	(get-coordinates 0)
	(print-board board horizontal-edges vertical-edges rows colomns)
	(if (check-if-completed) (quit-current-game) (get-input))
)

(defun quit-current-game()
	(terpri)
	(princ "Ending current game")
	(terpri)
	(print-board board horizontal-edges vertical-edges rows colomns)
	
)
(defun game-finished()
	(princ "You successfully finished the current game")
	(terpri)
	(print-board board horizontal-edges vertical-edges rows colomns)
	(terpri)
	(princ "Enter 1 to start a new game or any other key to exit")
	(terpri)
	(cond ((equal 1 (read) ) (start-game)))
)

(defun get-input()
	(princ "Press 1 to make an edge")
	(terpri)
	(princ "Press 2 to delete an edge")
	(terpri)
	(princ "Press 3 to quit the game")
	(terpri)
	(setq input (read))
	(cond ((= 1 input) (make-edge)) 
				((= 2 input) (delete-edge))
				((= 3 input) (quit-current-game))
				( t (princ "Enter correct values")(terpri)(print-board board horizontal-edges vertical-edges rows colomns)(get-input))
	))
	
(defun display-rules()
	(princ "Each puzzle consists of a rectangular lattice of dots with some clues in various places. The object is to link adjacent dots so:")
	(terpri)
	(princ "1. The value of each clue equals the number of links surrounding it.") 
	(terpri)
	(princ "2. Empty squares may be surrounded by any number of links.")
	(terpri)
	(princ "3. When completed, the solution forms a single continuous loop with no crossings or branches.")
	(terpri)
	(princ "Press 1 to start the game or any other key to quit")
	(terpri)
	(cond ((equal 1 (read) ) (start-game)))
)
(defun start-game()
	(princ "Enter filename: ")
	(terpri)
	(setq filename (read-line))
	(read-file filename)
	(print-board board horizontal-edges vertical-edges rows colomns)
	(terpri)
	(get-input))

(defun slither()
	(display-rules)
		
)
