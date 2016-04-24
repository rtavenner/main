; player.lisp

(defclass player () (
  (x    :type float :reader xpos     :initarg :x)  ; +x is East
  (y    :type float :reader ypos     :initarg :y)  ; +y is up
  (z    :type float :reader zpos     :initarg :z)  ; -z is North
  (alt  :type float :reader altitude :initform 0)  ; +alt is up, when facing forward
  (azi  :type float :reader azimuth  :initform 0)  ; +azi is right from -z
  (vlat :type float :initform 0)
  (vlon :type float :initform 0)
  (vver :type float :initform 0)
  (valt :type float :initform 0)
  (vazi :type float :initform 0)
  ))

(defmethod describe-object ((self player) stream)
  (format stream "~6,2f ~6,2f ~6,2f  ~6,2f ~6,2f~%"
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

