
(defmacro abbrev (long short) 
  `(defmacro ,short (&rest args) 
    `(,',long ,@args)))

(defconstant PHI (/ (+ (sqrt 5) 1) 2))
(defconstant PHI-2 (- 2 PHI))
(defconstant PHI-1 (- PHI 1))
(defconstant PHI2 (+ PHI 1))

(let ((fiblist (make-array 2 :initial-contents '(0 1) :adjustable t))) 
  (defun fib (n)
    (let ((temp (length fiblist)))
      (adjust-array fiblist (max temp (1+ n)))
      (do ((i temp (1+ i)))
        ((> i n) nil)
        (setf (aref fiblist i) (+ (aref fiblist (- i 1)) (aref fiblist (- i 2)))))
      (aref fiblist n))))

(defmethod greaterfib ((n integer)) ;gives a number x s.t. (fib x) > n
  (let ((temp (if (< n 2)
          2
          (1+ (ceiling (log n phi))))))
  (if (>= n (fib temp)) (1+ temp) temp)))
(defmethod toBasePhi ((n integer) &optional (p (- (greaterfib n) 2))) 
  (do* ((i (+ p 2) (- i 1)) (acc nil (cons (>= num (fib i)) acc)) (num n (if (first acc) (- num (fib i)) num))) 
    ((= i 2) (make-array (length acc) :initial-contents acc))))
(defmethod fromBasePhi ((places simple-vector)) 
  (do ((i 0 (+ i 1)) (acc 0 (if (svref places i) (+ acc (fib (+ i 2))) acc))) 
    ((= i (length places)) acc)))

(defconstant worldheight 256)


(defclass column ()
  (
    (coord 
      :accessor coord
      :initarg :coord
      :type integer)
    (tiles 
      :accessor tiles
      ; :initform (let ((c (make-array worldheight)))
        ; (do ((i 0 (+ 1 i)))
          ; ((= i worldheight) c)
          ; (setf (svref c i) (make-instance 'gameblock :id (mod i 3)))))
      :type simple-vector)))

(defmethod initialize-instance :after 
  ((c column) &rest initargs)
  (declare (ignore initargs))
  (with-slots (coord tiles) c
    (setf tiles 
      (let ((ca (make-array worldheight)))
        (do ((i 0 (+ 1 i)))
          ((= i worldheight) ca)
          (setf (svref ca i) (make-instance 'gameblock :id 
            (if (= i 1)
              (if (or (= coord 0) (not (svref (toBasePhi coord) 0)))
                2
                1)
              (if (= i 2)
                (if (or (= coord 0) (not (svref (toBasePhi coord) 0)))
                  0
                  2)
                0)))))))))

(defclass gameblock ()
  ((blockid 
      :initform 0
      :initarg :id
      :accessor id
      :type integer)))



(defmethod equalends ((a simple-vector) (b simple-vector))
  (let ((al (length a)) (bl (length b)))
    (do ((i 1 (1+ i)))
      ((or (> i al) (> i bl)) t)
      (if (not (eql (svref a (- al i)) (svref b (- bl i))))
        (return nil)))))



(defvar *coords* (make-hash-table :test #'equal))
(defvar *neighbors* (make-hash-table :test #'equal))
(defvar *world* (make-array 0 :adjustable t))

(defun getcolumn (n &optional (insist t))
  (if (>= n (length *world*))
    (if insist
      (setf *world* (adjust-array *world* (fib (greaterfib n)) :initial-element nil))
      (return-from getcolumn nil)))
  ; (print (length *world*))
  (let ((c (aref *world* n)))
    (if c
      c
      (if insist (setf (aref *world* n) (make-instance 'column :coord n))))))

(do
  ((i 0 (1+ i)))
  ((= i 610) nil)
  (getcolumn i))




; (defclass Penrose ()
;   ((dart
;       :accessor dartp
;       :initarg :dart
;       :initform nil
;       :type boolean)
;     (lvl ;0 = 1 tile
;       :accessor lvl
;       :initarg :lvl
;       :initform 7
;       :type integer
;       )
;     (elts
;       :accessor elts
;       :initarg :elts ;probably wont work
;       :type simple-vector)))
; (defmethod initialize-instance :after 
;   ((p Penrose) &rest initargs)
;   (declare (ignore initargs))
;   (with-slots (dart lvl elts) p
;     (setf elts 
;       (make-array (fib (+ (* 2 lvl) 
;           (if dart 1 2))) :initial-element nil))))



(defmethod modify ((arr simple-vector) &rest args) ;DESTRUCTIVE! input array and dotted lists (place.new)
  (dolist (arg args arr)
    (setf (svref arr (car arg)) (cdr arg))))


(defmethod Neighbor ((number integer) (side integer))
  (let ((n (gethash (+ (* 3 number) side) *neighbors*))) 
    (if n
      n
      (setf (gethash (+ (* 3 number) side) *neighbors*) 
        (labels
          ((NeighborHelper (places side n) 
            (if (>= (+ n 2) (length places))
              (if (and (= side 0) (or (= n (length places)) (and (not (svref places n)) (or (= (+ n 1) (length places)) (not (svref places (+ n 1)))))))
                (return-from Neighbor nil)
                (NeighborHelper (concatenate 'simple-vector places '(nil nil)) side n))
              (case side
                (0 
                  (if (svref places n) 
                    (if (svref places (+ n 2)) 
                      (modify (NeighborHelper places 1 (+ n 2)) (cons n nil) (cons (+ n 1) t))
                      (NeighborHelper places 2 (+ n 2)))
                    (if (svref places (+ n 1)) 
                      (let ((nb (NeighborHelper places 1 (+ n 2)))) 
                        (if (svref nb (+ 2 n)) 
                          (modify nb (cons n t) (cons (+ n 1) nil))
                          nb))
                      (NeighborHelper places 0 (+ n 2)))))
                (1 
                  (if (svref places n) 
                    (modify (copy-seq places) (cons n nil))
                    (if (svref places (+ n 1)) 
                      (NeighborHelper places 2 (+ n 2))
                      (modify (copy-seq places) (cons n t)))))
                (2 
                  (if (svref places n) 
                    (NeighborHelper places 0 (+ n 2))
                    (if (svref places (+ n 1)) 
                      (modify (copy-seq places) (cons (+ n 1) nil))
                      (if (svref places (+ n 2))
                        (NeighborHelper places 2 (+ n 2))
                        (modify (copy-seq places) (cons (+ n 1) t))))))))))
                  (NeighborHelper (toBasePhi number) side 0))))))




; (defclass world ()
;   (
;     (Left
;       :accessor Left
;       :initform (make-instance 'Penrose)
;       :type Penrose)
;     (Right
;       :accessor Right
;       :initform (make-instance 'Penrose)
;       :type Penrose)
;     (Lvl 
;       :accessor Lvl
;       :initarg :lvl
;       :initform 1
;       :type integer)))

; (defmethod getcolumn ((w world) (places simple-vector) right)
;   (do 
;     ((p (getHalf w right) (svref (elts p) (fromBasePhi (map simple-vector (lambda (x) (svref places (- (* 14 n) x))) '(14 13 12 11 10 9 8 7 6 5 4 3 2 1))))) (n (Lvl w) (- n 1)))
;     ((= n 0) p)
;     ))

; (defmethod expand-world ((w world))
;   (with-slots (Left Right lvl) w
;     (setf Left 
;       (let ((p (make-instance 'Penrose)))
;         (setf (aref (elts p) 0) Left)
;         p))
;     (setf Right 
;       (let ((p (make-instance 'Penrose)))
;         (setf (aref (elts p) 0) Right)
;         p))
;     (setf Lvl (+ Lvl 1)))
;   w)

; (defmethod getHalf ((w world) r) (if r (Right w) (Left w)))

; (defmethod get ((w world) (half boolean) (place simple-vector))
;   (do ((n 0 (+ n 14)) (p (getHalf w) (elts )))))

(defmacro coordshelphelperhelper (crds a b)
  `(vector
    (* phi (+ 
      (* (svref (svref ,crds 0) 0) ,a) 
      (* (svref (svref ,crds 1) 0) ,b) 
      (* (svref (svref ,crds 2) 0) (- 1 ,a ,b))))
    (* (- phi) (+ 
      (* (svref (svref ,crds 0) 1) ,a) 
      (* (svref (svref ,crds 1) 1) ,b) 
      (* (svref (svref ,crds 2) 1) (- 1 ,a ,b))))))

(defun coordshelphelper (crds a b c) ;(a1.a2) (b1.b2) (c1.c2)
  (vector
    (coordshelphelperhelper crds (car a) (cdr a))
    (coordshelphelperhelper crds (car b) (cdr b))
    (coordshelphelperhelper crds (car c) (cdr c))))

(defmethod coordshelp ((places simple-vector) (n integer))
  (if (< (- (length places) n) 2)
      (if (or (= n (length places)) (not (svref places n)))
        (vector 
          (vector 0 -1) 
          (vector 0 phi-1) 
          (vector (cos (/ pi 10)) (/ phi-1 2)))
        (vector 
          (vector 0 phi) 
          (vector (cos (/ pi 10)) (/ phi-1 2)) 
          (vector 0 phi-1)))
      (if (and (not (= (+ n 2) (length places))) (svref places (+ n 2)))
        (if (svref places n)
          (coordshelphelper (coordshelp places (+ n 2)) '(0 . 1) '(0 . 0) (cons phi-2 phi-1))
          (coordshelphelper (coordshelp places (+ n 2)) '(1 . 0) (cons phi-2 phi-1) '(0 . 0)))
        (if (svref places (+ n 1))
          (coordshelphelper (coordshelp places (+ n 2)) '(0 . 1) '(0 . 0) (cons phi-2 0))
          (if (svref places n)
            (coordshelphelper (coordshelp places (+ n 2)) '(1 . 0) (cons phi-2 0) (cons phi-1 phi-2))
            (coordshelphelper (coordshelp places (+ n 2)) '(0 . 1) (cons phi-1 phi-2) (cons phi-2 0)))))))

(defmethod coords ((n integer))
  (let ((c (gethash n *coords*)))
    (if c
      c
      (setf (gethash n *coords*) (coordshelp (toBasePhi n) 0)))))

(defmethod dotprod (xy1 xy2 xy3 xy4) ;(#(x1 y1) to #(x2 y2)) dot (#(x3 y3) to #(x4 y4))
  (+ 
    (* 
      (- (svref xy1 0) (svref xy2 0)) 
      (- (svref xy3 0) (svref xy4 0))) 
    (* 
      (- (svref xy1 1) (svref xy2 1)) 
      (- (svref xy3 1) (svref xy4 1)))))

(defmacro cwp (angle point1 point2)
  `(< 0 
    (- 
      (* 
        (- (svref ,angle 0) (svref ,point1 0)) 
        (- (svref ,angle 1) (svref ,point2 1))) 
      (* 
        (- (svref ,angle 1) (svref ,point1 1)) 
        (- (svref ,angle 0) (svref ,point2 0))))))

(defmethod intri ((point simple-vector) (coords simple-vector))
  (let 
    (
      (a (cwp (svref coords 0) (svref coords 1) point))
      (b (cwp (svref coords 1) (svref coords 2) point))
      (c (cwp (svref coords 2) (svref coords 0) point)))
    (or (and a b c) (not (or a b c)))))

(defmethod scalecoords ((coords simple-vector) xfactor yfactor)
  (vector
    (* xfactor (svref coords 0))
    (* yfactor (svref coords 1))))

(defmethod scaletricoords ((coords simple-vector) xfactor yfactor)
  (vector
    (scalecoords (svref coords 0) xfactor yfactor)
    (scalecoords (svref coords 1) xfactor yfactor)
    (scalecoords (svref coords 2) xfactor yfactor)))

(defmethod chunkcoord (chunknum chunklvl)
  (scaletricoords (coords chunknum) (expt phi chunklvl) (expt (- phi) chunklvl)))

(defmethod tile ((crds simple-vector))
  (do 
    (
      (i (do ((i 0 (+ 1 i))) ((intri crds (chunkcoord 0 i)) i)) (- i 1)) 
      (acc nil 
        (cond
          ((intri (scalecoords crds phi (- phi)) (chunkcoord (fromBasePhi (make-array (+ 2 (length acc)) :initial-contents (cons nil (cons nil acc)))) (- i 0))) (cons nil (cons nil acc)))
          ((intri (scalecoords crds phi (- phi)) (chunkcoord (fromBasePhi (make-array (+ 2 (length acc)) :initial-contents (cons t   (cons nil acc)))) (- i 0))) (cons t   (cons nil acc)))
          ((intri (scalecoords crds phi (- phi)) (chunkcoord (fromBasePhi (make-array (+ 2 (length acc)) :initial-contents (cons nil (cons t   acc)))) (- i 0))) (cons nil (cons t   acc)))
          (t (error "Error in tile.")))))
    ((zerop i) (make-array (length acc) :initial-contents acc))))






(defmacro addhelper (n y lst)
  `(push (vector (svref (svref coords ,n) 0) ,y (svref (svref coords ,n) 1)) ,lst))

(defmethod addtris ((c column) vlist normlist idlist) ;vertex order problems
  (let ((c-array (tiles c)) (coords (coords (coord c))))
    (let 
      (
        (x0 (svref (svref coords 0) 0))
        (y0 (svref (svref coords 0) 1))
        (x1 (svref (svref coords 1) 0))
        (y1 (svref (svref coords 1) 1))
        (x2 (svref (svref coords 2) 0))
        (y2 (svref (svref coords 2) 1)))
      (let ((dir (> (- (* (- x1 x0) (- y2 y0)) (* (- y1 y0) (- x2 x0))) 0)))
        (do 
          ((i 0 (+ i 1))) 
          ((= i (- worldheight 1)) (cons vlist (cons normlist idlist)))
          (if (and (not (zerop (id (svref c-array i)))) (zerop (id (svref c-array (+ 1 i))))) 
            (progn
              (addhelper 0 (+ i 1) vlist)
              (addhelper (if dir 1 2) (+ i 1) vlist)
              (addhelper (if dir 2 1) (+ i 1) vlist)
              (push (id (svref c-array i)) idlist)
              (push #(0 1 0) normlist))
            (if (and (zerop (id (svref c-array i))) (not (zerop (id (svref c-array (+ 1 i)))))) 
              (progn
                (addhelper 0 (+ i 1) vlist)
                (addhelper (if dir 2 1) (+ i 1) vlist)
                (addhelper (if dir 1 2) (+ i 1) vlist)
                (push (id (svref c-array (+ i 1))) idlist)
                (push #(0 -1 0) normlist)))))))))

(defmacro addquadshelper (a b narray)
  `(do 
      ((i 0 (+ i 1))) 
      ((= i (- worldheight 1)) nil)
      (if (and (not (zerop (id (svref c-array i)))) (or (null ,narray) (zerop (id (svref (tiles ,narray) i))))) 
        (progn
          (addhelper (if dir ,b ,a) i vlist)
          (addhelper (if dir ,b ,a) (+ i 1) vlist)
          (addhelper (if dir ,a ,b) (+ i 1) vlist)
          (addhelper (if dir ,a ,b) i vlist)
          (push (id (svref c-array i)) idlist)
          (let* (
            (x (- (svref (svref coords 1) 0) (svref (svref coords 0) 0)))
            (y (- (svref (svref coords 1) 1) (svref (svref coords 0) 1)))
            (d (sqrt (+ (* x x) (* y y)))))
            (push (vector (/ (- y) d) 0 (/ x d)) normlist))))))

(defmethod addquads ((c column) vlist normlist idlist)
  (let
    (
      (n0 (Neighbor (coord c) 0))
      (n1 (Neighbor (coord c) 1))
      (n2 (Neighbor (coord c) 2)))
    (let
      (
        (c-array (tiles c))
        (n0-array (if n0 (getcolumn (fromBasePhi n0) nil)))
        (n1-array (if n1 (getcolumn (fromBasePhi n1) nil)))
        (n2-array (if n2 (getcolumn (fromBasePhi n2) nil)))
        (coords (coords (coord c))))
      (let 
        (
          (x0 (svref (svref coords 0) 0))
          (y0 (svref (svref coords 0) 1))
          (x1 (svref (svref coords 1) 0))
          (y1 (svref (svref coords 1) 1))
          (x2 (svref (svref coords 2) 0))
          (y2 (svref (svref coords 2) 1)))
        (let ((dir (> (- (* (- x1 x0) (- y2 y0)) (* (- y1 y0) (- x2 x0))) 0)))
          (addquadshelper 0 1 n0-array)
          (addquadshelper 1 2 n1-array)
          (addquadshelper 2 0 n2-array)
          (cons vlist (cons normlist idlist)))))))

(defmethod tiles ((n null)) nil)
; (let ((v (list)) (f (list)))
;   (print (addtris (make-instance 'column :coord 0) v f)))

; (print (coordshelphelper #(#(0 0) #(0 1) #(1 1)) (0.8 . 0.1) (0.1 . 0.8) (0.1 . 0.1)))

; (setf world1 (make-instance 'world))
; (print world1)
; (print (getHalf world1 t))
; (print (Right world1))
; (print (Lvl world1))
; (print (expand-world world1))
; (print (Left world1))
; (print (Right world1))
; (print (Lvl world1))
; (print (elts (Left world1)))

; (print (elts (make-instance 'Penrose :lvl 7 :dart nil)))
; (print (mapcar #'toBasePhi '(0 1 2 3 4 5 6 7 8 9 986)))

; (mapcar (lambda (x) 
;   (format t "~A~%~A~%~A~%~%" 
;     (ignore-errors (fromBasePhi (Neighbor x 0)))
;     (ignore-errors (fromBasePhi (Neighbor x 1)))
;     (ignore-errors (fromBasePhi (Neighbor x 2))))) '(0 1 2 3 4 5 6 7 8 9 10 11 12))

; (mapcar (lambda (x) 
;   (format t "~A~%" 
;     (coords (toBasePhi x 5)))) 
;     '(0 1 2 3 4 5 6 7 8 9 10 11 12))

; (print *coords*)

; (print (toBasePhi 10 8))
;(print (fib 8))
;(print (fib 16))
; (print (greaterfib 5))
; (print (type-of t))
; (print (NeighborHelper #(nil nil t nil t) 1 4))

; (coordshelp #(nil nil nil nil nil t) 4)
; (print *coordshelp*)
; (setf *coordshelp* 0)
; (coords #(nil nil nil nil nil nil nil t))
; (print *coordshelp*)
; (setf *coordshelp* 0)
; (coords #(nil nil nil nil nil nil nil nil nil t))
; (print *coordshelp*)
; (setf *coordshelp* 0)
; (print (Neighbor 2 0))

; (print (cwp #(0 0) #(0 1) #(1 0)))
; (print (mapcar (lambda (x) (intri x #(#(-1 0) #(2 1) #(1 0)))) 
;   '(
;     #(-2 -1)
;     #(-2 0)
;     #(-2 1)
;     #(-2 2)
;     #(-2 3)
;     #(-1 -1)
;     #(-1 0)
;     #(-1 1)
;     #(-1 2)
;     #(-1 3)
;     #(0 -1)
;     #(0 0)
;     #(0 1)
;     #(0 2)
;     #(0 3)
;     #(1 -1)
;     #(1 0)
;     #(1 1)
;     #(1 2)
;     #(1 3)
;     #(2 -1)
;     #(2 0)
;     #(2 1)
;     #(2 2)
;     #(2 3))))

; (print (tile #(5 5)))

; (do 
;   ((i 0.1 (+ i 0.1)))
;   ((> i 5) nil)
;   (do 
;     ((j -5 (+ j 0.1)))
;     ((> j 5) nil)
;     (format t "~A" (mod (fromBasePhi (tile (vector i j))) 10)))
;   (format t "~%"))