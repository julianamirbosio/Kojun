;;; LISP - Resolvedor de Kojun com backtracking funcional

(defun make-copy (grid)
  (mapcar #'copy-seq grid))

(defun grid-dimensions (grid)
  (list (length grid) (length (car grid))))

(defun find-empty (grid)
  (loop for i from 0 below (length grid) do
    (loop for j from 0 below (length (car grid)) do
      (when (= (aref (nth i grid) j) 0)
        (return-from find-empty (list i j)))))
  nil)

(defun region-size (regions region-id)
  (count region-id (apply #'append regions)))

(defun region-cells (regions region-id)
  (loop for i from 0 below (length regions) append
    (loop for j from 0 below (length (car regions))
          when (= (aref (nth i regions) j) region-id)
          collect (list i j))))

(defun get-cell (grid r c)
  (when (and (>= r 0) (< r (length grid))
             (>= c 0) (< c (length (car grid))))
    (aref (nth r grid) c)))

(defun valid? (grid regions r c val region-cells)
  (let* ((region-id (aref (nth r regions) c))
         (adjacent-valid?
           (every (lambda (offset)
                    (let ((cell (get-cell grid (+ r (car offset)) (+ c (cadr offset)))))
                      (or (null cell) (/= cell val))))
                  '((-1 0) (1 0) (0 -1) (0 1))))
         (unique-in-region?
           (every (lambda (pos)
                    (/= (get-cell grid (car pos) (cadr pos)) val))
                  (cdr (assoc region-id region-cells :test #'equal))))
         (vertical-order?
           (let ((up (get-cell grid (1- r) c))
                 (down (get-cell grid (1+ r) c))
                 (up-region (get-cell regions (1- r) c))
                 (down-region (get-cell regions (1+ r) c)))
             (and (or (null up) (and (/= up val) (or (/= up-region region-id) (> up val))))
                  (or (null down) (and (/= down val) (or (/= down-region region-id) (< val down)))))))
    (and adjacent-valid? unique-in-region? vertical-order?)))

(defun update-grid (grid r c val)
  (let ((new-grid (make-copy grid)))
    (setf (aref (nth r new-grid) c) val)
    new-grid))

(defun solve-kojun (grid regions)
  (let* ((region-cells (mapcar (lambda (id)
                                 (cons id (region-cells regions id)))
                               (remove-duplicates (apply #'append regions))))
         (backtrack (lambda (g)
                      (let ((pos (find-empty g)))
                        (if (null pos)
                            g
                            (let* ((r (car pos)) (c (cadr pos))
                                   (region-id (aref (nth r regions) c))
                                   (max-val (region-size regions region-id)))
                              (loop for v from 1 to max-val
                                    for new-g = (update-grid g r c v)
                                    when (valid? g regions r c v region-cells)
                                      do (let ((result (funcall backtrack new-g)))
                                           (when result (return result))))
                              nil)))))
    (funcall backtrack grid)))
