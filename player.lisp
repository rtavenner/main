; player.lisp


; returns a function that gives fractional seconds since it was last called
(defun diff-time ()
  (multiple-value-bind (sec usec) (get-time-of-day)
    (lambda ()
      (multiple-value-bind (new-sec new-usec) (get-time-of-day)
        (prog1
          (+ (- new-sec sec) (* 0.000001 (- new-usec usec)))
          (setq sec  new-sec
                usec new-usec) )))))

(defclass player () (
  (x    :reader xpos       :initarg :x)  ; +x is East
  (y    :reader ypos       :initarg :y)  ; +y is up
  (z    :reader zpos       :initarg :z)  ; -z is North
  (alt  :reader altitude   :initform 0)  ; +alt is up, when facing forward
  (azi  :reader azimuth    :initform 0)  ; +azi is right from -z
  (vlat :initform 0)
  (vlon :initform 0)
  (vver :initform 0)
  (valt :initform 0)
  (vazi :initform 0)
  ))

(defmethod print ((self player))
  (format t "[~A, ~A, ~A] [~A, ~A]~%"
    (slot-value self 'x)
    (slot-value self 'y)
    (slot-value self 'z)
    (slot-value self 'alt)
    (slot-value self 'azi)
  ))

(defmethod update ((self player) dt)
  (let* ((dt_vlon (* dt (slot-value self 'vlon)))
         (dt_vlat (* dt (slot-value self 'vlat)))
         (dt_vver (* dt (slot-value self 'vver)))
         (dt_valt (* dt (slot-value self 'valt)))
         (dt_vazi (* dt (slot-value self 'vazi)))
         (sin_azi (sin (slot-value self 'azi)))
         (cos_azi (cos (slot-value self 'azi))) )
      (incf (slot-value self 'x) (+ (* dt_vlat cos_azi) (* dt_vlon sin_azi)))
      (incf (slot-value self 'y) dt_vver)
      (incf (slot-value self 'z) (- (* dt_vlat sin_azi) (* dt_vlon cos_azi)))
      (incf (slot-value self 'alt) dt_valt)
      (incf (slot-value self 'azi) dt_vazi)
    )
  )

(defmethod incr-lon ((self player) value)
  (incf (slot-value self 'vlon) value)
  )

(defmethod incr-lat ((self player) value)
  (incf (slot-value self 'vlat) value)
  )

(defmethod incr-ver ((self player) value)
  (incf (slot-value self 'vver) value)
  )

(defmethod incr-alt ((self player) value)
  (incf (slot-value self 'valt) value)
  )

(defmethod incr-azi ((self player) value)
  (incf (slot-value self 'vazi) value)
  )

