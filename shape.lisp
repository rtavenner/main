; shape.lisp

; (defclass shape () (
;   ; (x    :reader xpos       :initarg :x)  ; +x is East
;   ; (y    :reader ypos       :initarg :y)  ; +y is up
;   ; (z    :reader zpos       :initarg :z)  ; -z is North
;   ; (alt  :reader altitude   :initform 0)  ; +alt is up, when facing forward
;   ; (azi  :reader azimuth    :initform 0)  ; +azi is right from -z
;   ; (vlat :initform 0)
;   ; (vlon :initform 0)
;   ; (vver :initform 0)
;   ; (valt :initform 0)
;   ; (vazi :initform 0)
;   ))

; (defmethod draw ((self shape))
; )




(defun make-draw-octahedron ()
                  ;      0     1 +X       2     3 +Y       4      5 +Z
  (let ((vertices #( (-1 0 0) (1 0 0) (0 -1 0) (0 1 0) (0 0 -1) (0 0 1) ))
        (faces '( ; indices normal
          ((1 3 5) ( 1  1  1) (0.000000 0.166667 0.083333) (0.25 0.25 0.0)) ; top
          ((0 5 3) (-1  1  1) (0.083333 0.250000 0.166667) (0.25 0.25 0.0)) ; top
          ((3 1 4) ( 1  1 -1) (0.166667 0.333333 0.250000) (0.25 0.25 0.0)) ; top
          ((4 0 3) (-1  1 -1) (0.250000 0.416667 0.333333) (0.25 0.25 0.0)) ; top
          ((5 2 1) ( 1 -1  1) (0.333333 0.500000 0.416667) (0.25 0.25 0.0)) ; bottom
          ((5 0 2) (-1 -1  1) (0.416667 0.583333 0.500000) (0.25 0.25 0.0)) ; bottom
          ((2 4 1) ( 1 -1 -1) (0.500000 0.666667 0.583333) (0.25 0.25 0.0)) ; bottom
          ((0 4 2) (-1 -1 -1) (0.583333 0.750000 0.666667) (0.25 0.25 0.0)) ; bottom

          )) )

    (lambda (x y z uoff voff tex)
      (gl:bind-texture  :texture-2d tex)
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)

      (gl:push-matrix)
      (gl:translate x y z)

      (loop for (indices normal us vs) in faces do
        (apply 'gl:normal normal)
        (gl:begin :triangles)

        (map 'nil
          (lambda (index u v)
            (gl:tex-coord (+ u uoff) (+ v voff))
            (apply 'gl:vertex (aref vertices index)) )
          indices us vs )

        (gl:end))

      (gl:pop-matrix) )))


(defun make-draw-cube ()
  (let ((vertices #( (0 0 0) (0 1 0) (1 1 0) (1 0 0) (0 0 1) (0 1 1) (1 1 1) (1 0 1) ))
        (faces '( ; indices normal
          ((5 6 2 1) ( 0  1  0) (0.000000 0.166667 0.166667 0.000000) (0.25 0.25 0.0 0.0)) ; top
          ((4 7 6 5) ( 0  0  1) (0.166667 0.333333 0.333333 0.166667) (0.25 0.25 0.0 0.0)) ; front
          ((3 2 6 7) ( 1  0  0) (0.333333 0.500000 0.500000 0.333333) (0.25 0.25 0.0 0.0)) ; right
          ((1 2 3 0) ( 0  0 -1) (0.500000 0.666667 0.666667 0.500000) (0.25 0.25 0.0 0.0)) ; back
          ((4 5 1 0) (-1  0  0) (0.666667 0.833333 0.833333 0.666667) (0.25 0.25 0.0 0.0)) ; left
          ((0 3 7 4) ( 0 -1  0) (0.833333 1.000000 1.000000 0.833333) (0.25 0.25 0.0 0.0)) ; bottom
          )) )

    (lambda (x y z uoff voff tex)
      (gl:bind-texture  :texture-2d tex)
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)

      (gl:push-matrix)
      (gl:translate x y z)

      (loop for (indices normal us vs) in faces do
        (apply 'gl:normal normal)
        (gl:begin :quads)

        (map 'nil
          (lambda (index u v)
            (gl:tex-coord (+ u uoff) (+ v voff))
            (apply 'gl:vertex (aref vertices index)) )
          indices us vs )

        (gl:end))

      (gl:pop-matrix) )))

