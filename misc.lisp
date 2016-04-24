; misc.lisp



; returns a function that gives fractional seconds since it was last called
(defun diff-time ()
  (multiple-value-bind (sec usec) (get-time-of-day)
    (lambda ()
      (multiple-value-bind (new-sec new-usec) (get-time-of-day)
        (prog1
          (+ (- new-sec sec) (* 0.000001 (- new-usec usec)))
          (setq sec  new-sec
                usec new-usec) )))))


; (setf tn (gl:gen-textures 1))
; (gl:delete-textures (list tn))
; (sdl-image:load-image "lisplogo_alien_256.png")  ; returns sdl:surface


   

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

