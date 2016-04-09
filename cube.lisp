(require 'cl-opengl)
(require 'cl-glu)
(require 'lispbuilder-sdl)
 
(unless (boundp '+cube-vertices+) (
  (defconstant +window-width+  600)
  (defconstant +window-height+ 600)

  (defconstant +cube-vertices+
    #(#(0 0 0)
      #(0 1 0)
      #(1 1 0)
      #(1 0 0)
      #(0 0 1)
      #(0 1 1)
      #(1 1 1)
      #(1 0 1)))
   
  (defconstant +cube-faces+
    '((#(4 7 6 5) #(0 0 1))
      (#(5 6 2 1) #(0 1 0))
      (#(1 2 3 0) #(0 0 -1))
      (#(0 3 7 4) #(0 -1 0))
      (#(4 5 1 0) #(-1 0 0))
      (#(3 2 6 7) #(1 0 0))))
  ))

(defun draw-figure (verts faces)
  (labels (
    (set-normal (n)
             (gl:normal (aref n 0) (aref n 1) (aref n 2)))
    (set-vertex (index)
             (let ((v (aref verts index)))
               (gl:vertex (aref v 0) (aref v 1) (aref v 2))))
    (draw-face (vertex-indices normal)
             (set-normal normal)
             (gl:begin :quads)
             (map 'nil #'set-vertex vertex-indices)
             (gl:end))
    )
    (map 'nil #'(lambda (x) (draw-face (first x) (second x))) faces) ))
 
 
(defun draw-frame (rotx roty rotz)
  (gl:matrix-mode :modelview)
  (gl:push-matrix)

  (gl:translate (aref eye 0)
                (aref eye 1)
                (aref eye 2))

  ; (gl:translate 0 0 0)


  (gl:translate 0.5 0.5 0.5)
  (gl:rotate rotx 1 0 0)
  (gl:rotate roty 0 1 0)
  (gl:rotate rotz 0 0 1)
  (gl:translate -0.5 -0.5 -0.5)
  (gl:call-list obj1)
  ; (draw-figure +cube-vertices+ +cube-faces+)
  ; (gl:translate -1.0 -1.0 -1.0)
  ; (gl:call-list obj1)
  ; (print rotx)

  (gl:pop-matrix))
   
(defvar eye  #(0.0 0.0 0.0 0 0 0))  ; x y z  rotx roty rotz
(defvar cmds #(0 0))  ; fwd/back left/right
(defvar obj1)
; (defvar surf)

(defun start ()
  (let ((rotx 0)
        (roty 0)
        (rotz 0))
    (sdl:with-init ()
      ; (gl:viewport 0 0 +window-width+ +window-height+)

      ; (sdl:set-gl-attribute :sdl-gl-depth-size   16)

      (sdl:window +window-width+ +window-height+
          :title-caption "c.lisp"
          ; :flags (logior sdl:sdl-no-frame sdl:sdl-opengl sdl:sdl-doublebuf)
          :opengl t
          :opengl-attributes '((:sdl-gl-depth-size   16)
                               (:sdl-gl-doublebuffer 1))
        )


      (setf (sdl:frame-rate) 30)

      (gl:viewport 0 0 +window-width+ +window-height+)

      (gl:matrix-mode :projection)
      (gl:load-identity)
      (glu:perspective 50 (/ +window-height+ +window-width+) 0.2 20.0)
      (glu:look-at  0.0 1.5 2     ; camera
                    0.0 1.0 -2    ; object
                    0 1 0)        ; zenith
 
      (gl:matrix-mode :modelview)
      (gl:load-identity)
 
      (gl:draw-buffer :back)
      (gl:clear-color 0.2 0.2 0.1 0)
      (gl:cull-face :back)
      (gl:polygon-mode :front :fill)
      (gl:shade-model :flat)

      (gl:light :light0 :position #( 0 0  1   0))
      (gl:light :light0 :diffuse  #( 1 0  0   0))
      (gl:light :light1 :position #(-1 2 -0.5 0))
      (gl:light :light1 :diffuse  #( 0 1  0   0))
      (gl:enable :cull-face :depth-test
                 :lighting :light0 :light1)
 
      (setf obj1 (gl:gen-lists 1))
      (gl:with-new-list (obj1 :compile)
        (gl:material :front :ambient-and-diffuse #(0.7 0.7 0.7 0.4))
        (draw-figure +cube-vertices+ +cube-faces+) )
 
      ; (sdl:delay 1000)

      (sdl:with-events ()
        (:quit-event () t)

        (:video-expose-event () (sdl:update-display))

        ; (:mouse-motion-event (:x mouse-x :y mouse-y)
        ;   (sdl:update-display)
        ;   )

        (:key-down-event (:key key)
          (case key
            (:sdl-key-escape (sdl:push-quit-event))
            (:sdl-key-w (incf (aref cmds 0)  0.1))
            (:sdl-key-a (incf (aref cmds 1)  0.1))
            (:sdl-key-s (incf (aref cmds 0) -0.1))
            (:sdl-key-d (incf (aref cmds 1) -0.1))
          ))

        (:key-up-event (:key key)
          (case key
            (:sdl-key-w (incf (aref cmds 0) -0.1))
            (:sdl-key-a (incf (aref cmds 1) -0.1))
            (:sdl-key-s (incf (aref cmds 0)  0.1))
            (:sdl-key-d (incf (aref cmds 1)  0.1))
          ))

        (:idle

          ; (sdl:with-surface (surf)
          ;   (print surf)
          (print sdl:*DEFAULT-SURFACE*)

          (incf (aref eye 0) (aref cmds 1))
          (incf (aref eye 2) (aref cmds 0))
          (setq roty (mod (+ roty 0.7) 360.0))
          (gl:clear :color-buffer :depth-buffer)
          (draw-frame rotx roty rotz)
          (sdl:update-display)
        )
      ))))

(start)
