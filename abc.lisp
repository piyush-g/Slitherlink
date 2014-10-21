(defparameter rows 5)
(defparameter colomns 5)
(defparameter matrix nil)
(princ "Enter filename: ")


(defun construct-row(colomn)
	(cond ((>= 1 colomn) '(nil)) 
				( t (append '(nil) (construct-row (1- colomn))))))

(defun construct-matrix(row colomn)
	(cond ((>= 1 row) (list (construct-row colomn)))
				(t (cons (construct-row colomn) (construct-matrix (1- row) colomn)))))

( defun element(board i j)
	(nth j (nth i board)))

(defun set-element(board i j value)
	(setf (nth j (nth i board)) value))

( defun read-file(filename)
	(let ((in (open filename :if-does-not-exist nil)))
	(setq	rows (read in))
	(setq	colomns (read in))
	(setq matrix (construct-matrix rows colomns))
  (when in
    (loop for i = (read in nil)
    			for j = (read in nil)
    			for val = (read in nil)
    			while val  do (set-element matrix i j val))
    (close in))))
(setq filename (read-line))
(read-file filename)

