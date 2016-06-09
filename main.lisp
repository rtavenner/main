


; (require 'cl-ppcre)
(require 'cl-opengl)
(require 'cl-glu)
(require 'lispbuilder-sdl)
(require 'lispbuilder-sdl-image)
; (require 'black-tie)  ; simplex-noise

(load "player.lisp")
(load "PenroseCraft.lisp")

(defmacro lambdatexcoord (u v)
  `(lambda () (gl:tex-coord ,u ,v)))

(defmacro vltc3 (u1 v1 u2 v2 u3 v3)
  `(vector (lambdatexcoord ,u1 ,v1) (lambdatexcoord ,u2 ,v2) (lambdatexcoord ,u3 ,v3)))

(defmacro vltc4 (u1 v1 u2 v2 u3 v3 u4 v4)
  `(vector (lambdatexcoord ,u1 ,v1) (lambdatexcoord ,u2 ,v2) (lambdatexcoord ,u3 ,v3) (lambdatexcoord ,u4 ,v4)))
 
(defconstant tritextable 
  (vector
    nil
    (vltc3 0 (/ 2 16) 0 (/ 1 16) (/ 1 16) (/ 1 16))
    (vltc3 (/ 1 16) (/ 1 16) (/ 2 16) (/ 1 16) (/ 3 32) (/ 2 16))
    (vltc3 0 (/ 2 16) 0 (/ 1 16) (/ 1 16) (/ 1 16))
    ; (vltc3 0 (/ 1 256) 0 0 (/ 1 256) 0)
    ))

(defconstant quadtextable 
  (vector 
    nil
    (vltc4 0 (/ 2 16) 0 (/ 1 16) (/ 1 16) (/ 1 16) (/ 1 16) (/ 2 16))
    (vltc4 0 (/ 1 16) 0 0 (/ 1 16) 0 (/ 1 16) (/ 1 16))
    (vltc4 (/ 1 16) (/ 1 16) (/ 1 16) 0 (/ 2 16) 0 (/ 2 16) (/ 1 16))))




(defun load-a-texture (filename)
  (let ((texture (car (gl:gen-textures 1)))
        (image (sdl-image:load-image filename)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)

    (sdl-base::with-pixel (pix (sdl:fp image))
      ;; we should probably be a bit more intelligent about this, but this
      ;; handles some common cases
      (let ((texture-format (ecase (sdl-base::pixel-bpp pix)
                              (3 :rgb)
                              (4 :rgba))))

        ;; we should also handle this properly, by adjusting the
        ;; settings of gl:pixel-store
        (assert (and (= (sdl-base::pixel-pitch pix)
                        (* (sdl:width image) (sdl-base::pixel-bpp pix)))
                     (zerop (rem (sdl-base::pixel-pitch pix) 4))))
        (gl:tex-image-2d :texture-2d 0 :rgba
                         (sdl:width image) (sdl:height image)
                         0
                         texture-format
                         :unsigned-byte (sdl-base::pixel-data pix))))
    texture))

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

(defun draw-tris (verts norms ids)
  (when verts
    (gl:normal (svref (car norms) 0) (svref (car norms) 1) (svref (car norms) 2))
    (gl:begin :triangles)
    (funcall (svref (svref tritextable (car ids)) 0))
    (gl:vertex (svref (car verts) 0) (svref (car verts) 1) (svref (car verts) 2))
    (funcall (svref (svref tritextable (car ids)) 1))
    (gl:vertex (svref (cadr verts) 0) (svref (cadr verts) 1) (svref (cadr verts) 2))
    (funcall (svref (svref tritextable (car ids)) 2))
    (gl:vertex (svref (caddr verts) 0) (svref (caddr verts) 1) (svref (caddr verts) 2))
    (gl:end)
    (draw-tris (cdddr verts) (cdr norms) (cdr ids))))

(defun draw-quads (verts norms ids)
  (when verts
    (gl:normal (svref (car norms) 0) (svref (car norms) 1) (svref (car norms) 2))
    (gl:begin :quads)
    (funcall (svref (svref quadtextable (car ids)) 0))
    (gl:vertex (svref (car verts) 0) (svref (car verts) 1) (svref (car verts) 2))
    (funcall (svref (svref quadtextable (car ids)) 1))
    (gl:vertex (svref (cadr verts) 0) (svref (cadr verts) 1) (svref (cadr verts) 2))
    (funcall (svref (svref quadtextable (car ids)) 2))
    (gl:vertex (svref (caddr verts) 0) (svref (caddr verts) 1) (svref (caddr verts) 2))
    (funcall (svref (svref quadtextable (car ids)) 3))
    (gl:vertex (svref (cadddr verts) 0) (svref (cadddr verts) 1) (svref (cadddr verts) 2))
    (gl:end)
    (draw-quads (cddddr verts) (cdr norms) (cdr ids))))


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

  ; (gl:light :light0 :position #( 0 0  1   0))
  ; (gl:light :light0 :diffuse  #( 1 0  0   0))
  ; (gl:light :light1 :position #(-1 2 -0.5 0))
  ; (gl:light :light1 :diffuse  #( 0 1  0   0))

  (gl:call-list obj1)

  (gl:pop-matrix) )


(defun redraw ()
  (gl:with-new-list (obj1 :compile)
        (gl:matrix-mode :modelview)
        (gl:push-matrix)
        ; (gl:material :front :ambient-and-diffuse #(0.7 0.7 0.7 0.4))

        (gl:push-matrix)
        (gl:bind-texture  :texture-2d tex)
        (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
        (do
          ((i 0 (1+ i)))
          ((= i (length *world*)) nil)
          (let* ((c (getcolumn i))
               (a (addtris c (list) (list) (list)))
               (b (addquads c (list) (list) (list))))
            (draw-tris (car a) (cadr a) (cddr a))
            (draw-quads (car b) (cadr b) (cddr b))))
        (gl:pop-matrix)

        (gl:pop-matrix)))   

(defun start ()
  (let ((dt (diff-time))
       )

    (setf player (make-instance 'player :x 0 :y 0 :z 0))


    (sdl:with-init ()

      (sdl:window +window-width+ +window-height+
          :title-caption "PenroseCraft"
          ; :flags (logior sdl:sdl-no-frame sdl:sdl-opengl sdl:sdl-doublebuf)
          :opengl t
          :opengl-attributes '((:sdl-gl-depth-size   16)
                               (:sdl-gl-doublebuffer 1))
        )

      (defconstant tex (load-a-texture "Textures.png"))

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
      ; (gl:shade-model :flat)

      ; (gl:light :light0 :position #( 0 0  1   0))
      ; (gl:light :light0 :diffuse  #( 1 0  0   0))
      ; (gl:light :light1 :position #(-1 2 -0.5 0))
      ; (gl:light :light1 :diffuse  #( 0 1  0   0))
      (gl:enable :cull-face :depth-test
                :texture-2d)
              ; :lighting :light0 :light1)
 
      (setf obj1 (gl:gen-lists 1))
      (redraw)
 
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
            (:sdl-key-lshift (incr-ver  player -1.0))
            (:sdl-key-space  (incr-ver  player  1.0))
            (:sdl-key-up     (incr-alt  player -1.0))
            (:sdl-key-down   (incr-alt  player  1.0))
            (:sdl-key-left   (incr-azi  player -1.0))
            (:sdl-key-right  (incr-azi  player  1.0))

            (:sdl-key-0 
              (if (> (xpos player) 0.1)
                (let ((tile (frombasephi (tile (vector (xpos player) (zpos player))))))
                  (if (and (> (ypos player) 1) (< (ypos player) (+ 1 worldheight)))
                    (setblock tile (- (floor (ypos player)) 1) 0)))))
            (:sdl-key-1 
              (if (> (xpos player) 0.1)
                (let ((tile (frombasephi (tile (vector (xpos player) (zpos player))))))
                  (if (and (> (ypos player) 1) (< (ypos player) (+ 1 worldheight)))
                    (setblock tile (- (floor (ypos player)) 1) 1)))))
            (:sdl-key-2 
              (if (> (xpos player) 0.1)
                (let ((tile (frombasephi (tile (vector (xpos player) (zpos player))))))
                  (if (and (> (ypos player) 1) (< (ypos player) (+ 1 worldheight)))
                    (setblock tile (- (floor (ypos player)) 1) 2)))))
            (:sdl-key-3 
              (if (> (xpos player) 0.1)
                (let ((tile (frombasephi (tile (vector (xpos player) (zpos player))))))
                  (if (and (> (ypos player) 1) (< (ypos player) (+ 1 worldheight)))
                    (setblock tile (- (floor (ypos player)) 1) 3)))))
          ))

        (:key-up-event (:key key)
          (case key
            (:sdl-key-w      (incr-lon  player -1.0))
            (:sdl-key-s      (incr-lon  player  1.0))
            (:sdl-key-d      (incr-lat  player -1.0))
            (:sdl-key-a      (incr-lat  player  1.0))
            (:sdl-key-lshift (incr-ver  player  1.0))
            (:sdl-key-space  (incr-ver  player -1.0))
            (:sdl-key-up     (incr-alt  player  1.0))
            (:sdl-key-down   (incr-alt  player -1.0))
            (:sdl-key-left   (incr-azi  player  1.0))
            (:sdl-key-right  (incr-azi  player -1.0))
          ))

        (:idle
          (update player (funcall dt))
          ; (printme player)
          (gl:clear :color-buffer :depth-buffer)
          (draw-frame)
          (sdl:update-display)
          ; (when (< (random 1.0) 0.01)
          ;   (setblock 0 3 (- 1 (getblock 0 3))))
          ; (if (> (xpos player) 0.1)
          ;   (let ((tile (frombasephi (tile (vector (xpos player) (zpos player))))))
          ;     (unless (= (getblock tile 2) 1)
          ;       (setblock tile 2 1))))
          (if (> (xpos player) 0.1) 
            (let ((tile (frombasephi (tile (vector (xpos player) (zpos player)))))) 
              (when (> tile (length *world*))
                (getcolumn tile)
                (redraw))))
        )
      ))))
(defun getblock (x y &optional (insist t)) 
  (let ((c (getcolumn x insist)))
    (if c (id (svref (tiles c) y)))))
(defun setblock (x y id) 
  ; (format t "setting ~A ~A to ~A" x y id)
  (setf (id (svref (tiles (getcolumn x)) y)) id)
  (redraw))
(defun lookingAt ()
  (let 
    (
      (x (xpos player))
      (y (ypos player))
      (z (zpos player))
      (alt (altitude player))
      (azi (azimuth player)))
    '()))
(start)
;CTRL+B to run