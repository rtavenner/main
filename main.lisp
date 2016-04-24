(require 'cl-opengl)
(require 'cl-glu)
(require 'lispbuilder-sdl)
(require 'lispbuilder-sdl-image)


(load "misc.lisp")
(load "player.lisp")
(load "shape.lisp")
 
(defparameter +window-width+  600)
(defparameter +window-height+ 600)


(defun start ()
  (let ((dt (diff-time))
        (draw-cube (make-draw-cube))
        (draw-octahedron (make-draw-octahedron))
        (player (make-instance 'player :x 0 :y 0 :z 0))
        (tex1 nil)
        (obj1 nil) )

    (sdl:with-init ()

      (sdl:window +window-width+ +window-height+
          :title-caption "main.lisp"
          ; :flags (logior sdl:sdl-no-frame sdl:sdl-opengl sdl:sdl-doublebuf)
          :opengl t
          :opengl-attributes '((:sdl-gl-depth-size   16)
                               (:sdl-gl-doublebuffer 1)) )

      (setf (sdl:frame-rate) 30)
      (setf tex1 (load-a-texture "resource/strip.png"))
      

      (gl:viewport 0 0 +window-width+ +window-height+)

      (gl:matrix-mode :projection)
      (gl:load-identity)
      (glu:perspective 50 (/ +window-height+ +window-width+) 0.2 20.0)
 
      (gl:matrix-mode :modelview)
      (gl:load-identity)
 
      (gl:draw-buffer :back)
      (gl:clear-color 0.6 0.2 0.1 0)
      (gl:cull-face :back)
      (gl:polygon-mode :front :fill)
      (gl:shade-model :flat)

      (gl:light :light0 :position #( 0 0  1   0))
      (gl:light :light0 :diffuse  #( 1 0  0   0))
      (gl:light :light1 :position #(-1 2 -0.5 0))
      (gl:light :light1 :diffuse  #( 0 1  0   0))
      (gl:enable :cull-face :depth-test :texture-2d
                 :lighting :light0 :light1)

      (gl:disable :lighting)
      ; (gl:enable :blend :texture-2d :depth-test)
      ; (gl:blend-func :src-alpha :one)


      (setf obj1 (gl:gen-lists 1))
      (gl:with-new-list (obj1 :compile)
        (gl:material :front :ambient-and-diffuse #(0.7 0.7 0.7 0.4))
        (funcall draw-octahedron  0 3 -3 0.0 0.0 tex1)
        (funcall draw-cube  0 0 -5 0.0 0.0 tex1)
        (funcall draw-cube -2 0 -5 0.0 0.0 tex1)
        (funcall draw-cube -2 0 -4 0.0 0.0 tex1)
        (funcall draw-cube -2 0 -3 0.0 0.0 tex1)
        (funcall draw-cube -3 0 -5 0.0 0.0 tex1)
        (funcall draw-cube -3 0 -4 0.0 0.0 tex1)
        (funcall draw-cube -3 0 -3 0.0 0.0 tex1) )
 
      (sdl:with-events ()
        (:quit-event () 
          (gl:delete-textures (list tex1))
          t)

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
          ; (describe player)
          (gl:clear :color-buffer :depth-buffer)

          (gl:matrix-mode :modelview)
          (gl:push-matrix)

          (gl:rotate (* 57.297 (altitude player)) 1 0 0)
          (gl:rotate (* 57.297 (azimuth  player)) 0 1 0)

          (gl:translate (- (xpos player))
                        (- (ypos player))
                        (- (zpos player)) )

          (gl:call-list obj1)

          (gl:pop-matrix)
          (gl:flush)

          (sdl:update-display) )
      ))))

(start)
