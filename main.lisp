(require 'cl-opengl)
(require 'cl-glu)
(require 'lispbuilder-sdl)

(load "player.lisp")
 
(unless (boundp '+cube-vertices+) 
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



  (defconstant +prism-vertices+
    #(#(0 0 0)
      #(0 0 1)
      #(0 1 0)
      #(1 1 0)
      #(1 0 0)
      #(0 1 1)
      #(1 1 1)
      #(1 0 1)))

  (defconstant +prism-faces+
    '((#(4 7 6 5) #(0 0 1))
      (#(5 6 2 1) #(0 1 0))
      (#(1 2 3 0) #(0 0 -1))
      (#(0 3 7 4) #(0 -1 0))
      (#(4 5 1 0) #(-1 0 0))
      (#(3 2 6 7) #(1 0 0))))
  )

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
 

(defvar obj1 nil)
(defvar player)

(defun draw-frame ()

  (gl:matrix-mode :modelview)
  (gl:push-matrix)

  (gl:rotate (* 57.297 (altitude player)) 1 0 0)
  (gl:rotate (* 57.297 (azimuth  player)) 0 1 0)

  (gl:translate (- (xpos player))
                (- (ypos player))
                (- (zpos player)))

  (gl:call-list obj1)

  (gl:pop-matrix) )
   

(defun start ()
  (let ((dt (diff-time))
       )

    (setf player (make-instance 'player :x 0 :y 0 :z 0))


    (sdl:with-init ()

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
        ; (gl:matrix-mode :modelview)
        ; (gl:push-matrix)
        (gl:material :front :ambient-and-diffuse #(0.7 0.7 0.7 0.4))

        (gl:push-matrix)
        (gl:translate 0 0 -5)
        (draw-figure +cube-vertices+ +cube-faces+)
        (gl:pop-matrix)

        (gl:push-matrix)
        (gl:translate -2 0 -5)
        (draw-figure +cube-vertices+ +cube-faces+)
        (gl:pop-matrix)

        (gl:push-matrix)
        (gl:translate -2 0 -3)
        (draw-figure +cube-vertices+ +cube-faces+)
        (gl:pop-matrix)

        ; (gl:pop-matrix)
        )
 
      (sdl:with-events ()
        (:quit-event () t)

        (:video-expose-event () (sdl:update-display))

        ; (:mouse-motion-event (:x mouse-x :y mouse-y)
        ;   (sdl:update-display)
        ;   )

        (:key-down-event (:key key)
          (case key
            (:sdl-key-escape (sdl:push-quit-event))
            (:sdl-key-w      (incr-lon  player  1.0))
            (:sdl-key-s      (incr-lon  player -1.0))
            (:sdl-key-d      (incr-lat  player  1.0))
            (:sdl-key-a      (incr-lat  player -1.0))
            (:sdl-key-lctrl  (incr-ver  player  1.0))
            (:sdl-key-space  (incr-ver  player -1.0))
            (:sdl-key-up     (incr-alt  player  1.0))
            (:sdl-key-down   (incr-alt  player -1.0))
            (:sdl-key-left   (incr-azi  player -1.0))
            (:sdl-key-right  (incr-azi  player  1.0))
          ))

        (:key-up-event (:key key)
          (case key
            (:sdl-key-w      (incr-lon  player -1.0))
            (:sdl-key-s      (incr-lon  player  1.0))
            (:sdl-key-d      (incr-lat  player -1.0))
            (:sdl-key-a      (incr-lat  player  1.0))
            (:sdl-key-lctrl  (incr-ver  player -1.0))
            (:sdl-key-space  (incr-ver  player  1.0))
            (:sdl-key-up     (incr-alt  player -1.0))
            (:sdl-key-down   (incr-alt  player  1.0))
            (:sdl-key-left   (incr-azi  player  1.0))
            (:sdl-key-right  (incr-azi  player -1.0))
          ))

        (:idle
          (update player (funcall dt))
          (print player)
          (gl:clear :color-buffer :depth-buffer)
          (draw-frame)
          (sdl:update-display)
        )
      ))))

(start)
