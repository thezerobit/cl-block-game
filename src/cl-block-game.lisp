#|
  This file is a part of cl-block-game project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-block-game
  (:use :cl)
  (:import-from :fset
    #:seq
    #:convert
    #:@
    #:with
    #:with-last
    #:with-first
    #:less
    #:*fset-readtable*
    #:gmap
    #:with-default
    #:contains?
    #:subseq
    #:size
    #:filter
    #:concat)
  (:shadowing-import-from :fset
    #:set-difference
    #:intersection
    #:reduce))
(in-package :cl-block-game)

(eval-when (:compile-toplevel :load-toplevel :execute) 
            (setf *readtable* *fset-readtable*))

(defmacro -> (x &optional (form nil form-supplied-p) &rest more)
  (if form-supplied-p
    (if more
      `(-> (-> ,x ,form) ,@more)
      (if (listp form)
        `(,(car form) ,x ,@(cdr form))
        (list form x)))
    x))

;; random number generator that creates a closure which will act like the
;; builtin RANDOM function when funcalled with an argument, but it will
;; also return another generator that will produce the next value in the
;; series
(defun make-functional-rng (&optional random-state)
  (let ((rand-state (or random-state (make-random-state T))))
    (lambda (arg)
      (let ((copied-state (make-random-state rand-state)))
        (values (random arg copied-state)
                (make-functional-rng copied-state))))))

(defgeneric make-grid (w h init))

(defmethod make-grid (w h (init seq))
  #{| (:width w) (:height h) (:v init) |})

(defmethod make-grid (w h (init T))
  #{| (:width w) (:height h)
    (:v (convert 'seq (loop for i from 1 to (* w h) collecting init))) |})

(defun grid-ref (grid x y)
  (@ (@ grid :v) (+ x (* y (@ grid :width)))))

(defun do-grid-fun-aux (grid val fn x y width)
   (let ((v (@ grid :v)))
     (if (>= y (@ grid :height))
       val
       (if (>= x width)
         (do-grid-fun-aux grid val fn 0 (1+ y) width)
         (do-grid-fun-aux grid
                (funcall fn val (@ v (+ x (* y width))) x y)
                fn (1+ x) y width)))))

(defun do-grid-fun (grid val fn)
  (do-grid-fun-aux grid val fn 0 0 (@ grid :width)))

(defun make-piece (&rest body)
  (make-grid 4 4 (convert 'seq body)))

;; function that takes a game hash, and a number and returns the
;; game hash with updated random number generator and the generated
;; random number
(defun game-random (game arg)
  (multiple-value-bind (result new-rng) (funcall (@ game :rng) arg)
    (values (with game :rng new-rng) result)))

(defun random-piece (game)
  (multiple-value-bind (game random-value)
    (game-random game *count-tetraminos*)
    (values game (nth random-value *tetraminos*))))

(defun make-falling-block (game &key (x 0) (y 0))
  (multiple-value-bind (updated-game tetramino) (random-piece game)
    (values updated-game
            #{| (:x x) (:y y) (:tetramino tetramino) (:position 0) |})))

(defun falling-block-grid (falling-block)
  (nth (@ falling-block :position) (@ falling-block :tetramino)))

(defun rotate-falling-block (falling-block n)
  (let* ((len (length (@ falling-block :tetramino)))
         (new-pos (mod (+ n (@ falling-block :position)) len)))
    (with falling-block :position new-pos)))

(defun rotate-right (grid)
  (let* ((w (@ grid :width))
         (h (@ grid :height))
         (new-seq #[ ]))
    (make-grid w h 
               (do-grid-fun grid new-seq
                            (lambda (new-seq val x y)
                              (let ((new-x (- w y 1))
                                    (new-y x))
                                (with new-seq (+ new-x (* w new-y)) 
                                      val)))))))

(defun get-four-rotations (shape)
  (let* ((r1 (rotate-right shape))
         (r2 (rotate-right r1))
         (r3 (rotate-right r2)))
    (list shape r1 r2 r3)))

;; parameters

(defparameter *tetraminos*
  (list
    (get-four-rotations (make-piece
                          0 1 0 0
                          0 1 0 0
                          0 1 1 0
                          0 0 0 0))
    (get-four-rotations (make-piece
                          0 0 1 0
                          0 0 1 0
                          0 1 1 0
                          0 0 0 0))
    (list
      (make-piece
        0 0 0 0
        1 1 1 1
        0 0 0 0
        0 0 0 0)

      (make-piece
        0 1 0 0
        0 1 0 0
        0 1 0 0
        0 1 0 0))
    (list
      (make-piece
        0 0 1 0
        0 1 1 0
        0 1 0 0
        0 0 0 0)

      (make-piece
        0 0 0 0
        0 1 1 0
        0 0 1 1
        0 0 0 0))
    (list
      (make-piece
        0 1 0 0
        0 1 1 0
        0 0 1 0
        0 0 0 0)

      (make-piece
        0 0 0 0
        0 1 1 0
        1 1 0 0
        0 0 0 0))
    (list
      (make-piece
        0 0 0 0
        0 1 1 0
        0 1 1 0
        0 0 0 0))))

(defparameter *count-tetraminos* (length *tetraminos*))

(defparameter *key-delay-frames* 20)

(defparameter *key-repeat-frames* 5)

(defparameter *game-mode* :playing)

(defparameter *key-map*
  #{| (:sdl-key-up    :r-right)
      (:sdl-key-down  :down)
      (:sdl-key-left  :left)
      (:sdl-key-right :right)
      (:sdl-key-space :drop)
      (:sdl-key-c     :r-left)
      (:sdl-key-v     :r-right) |} )


(defun draw-string (string x y &key (color sdl:*white*) (bg sdl:*black*))
  (sdl:draw-string-shaded-* string x y color bg))

(defun draw-backdrop (grid offset-x offset-y width height)
  (sdl:draw-box (sdl:rectangle-from-edges-*
                  offset-x offset-y
                  (1- (* width (1+ (@ grid :width))))
                  (1- (* height (1+ (@ grid :height)))))
                :color sdl:*blue*))

(defun draw-grid (grid offset-x offset-y width height)
  (do-grid-fun grid nil
    (lambda (none val x y)
      (declare (ignore none))
      (let ((x1 (+ offset-x (* x width)))
            (y1 (+ offset-y (* y height)))
            (x2 (- (+ offset-x (* (+ 1 x) width)) 1))
            (y2 (- (+ offset-y (* (+ 1 y) height)) 1))
            (color (if (eq 1 val) sdl:*white* sdl:*black*)))
        (when (> val 0)
          (sdl:draw-box (sdl:rectangle-from-edges-* x1 y1 x2 y2)
            :color color))))))

(defun make-game ()
  #{| (:main-grid (make-grid 10 20 0))
      (:fb nil)
      (:frame-counter 0)
      (:drop-counter 0)
      (:active T)
      (:cleared-lines 0)
      (:level 1)
      (:keys-down (fset:map))
      (:rng (make-functional-rng)) |})

(defun draw-game (game offset-x offset-y width height)
  (draw-backdrop (@ game :main-grid) offset-x offset-y width height)
  (draw-grid (@ game :main-grid) offset-x offset-y width height)
  (let ((fb (@ game :fb)))
    (when fb
      (draw-grid (falling-block-grid fb)
                 (+ offset-x (* width (@ fb :x)))
                 (+ offset-y (* height (@ fb :y)))
                 width height)))
  (draw-string (format nil "Game Mode: ~a" *game-mode*) 120 10)
  (draw-string "Keys:" 120 30)
  (draw-string "r : reverse time" 120 40)
  (draw-string "p : pause time" 120 50)
  (draw-string "f : forward time" 120 60)

  (draw-string "up arrow : rotate block" 120 80)
  (draw-string "other arrows : move block" 120 90)
  (draw-string "space : drop block" 120 100)

  (draw-string (format nil "LEVEL: ~a" (@ game :level)) 30 1))

(defun drop-counter-reset (game)
  (with game :drop-counter 30))

(defun legal-block-position (game fb)
  (let* ((main-grid (@ game :main-grid)))
    (do-grid-fun (falling-block-grid fb) nil
                 (lambda (none elem x y)
                   (declare (ignore none))
                   (let ((x (+ x (@ fb :x)))
                         (y (+ y (@ fb :y))))
                     (when (and (/= elem 0)
                             (or (< x 0)
                                 (< y 0)
                                 (>= x (@ main-grid :width))
                                 (>= y (@ main-grid :height))
                                 (/= (grid-ref main-grid x y) 0)))
                       (return-from legal-block-position nil)))))
    T))

;; main is a grid hash, other is a grid hash
(defun grid-combine-fun (main other offset-x offset-y)
  (let ((w (@ main :width))
        (h (@ main :height)))
    (with main :v
      (do-grid-fun other (@ main :v)
        (lambda (grid-vector elem x y)
          (let* ((target-x (+ x offset-x))
                (target-y (+ y offset-y))
                (target-offset (+ target-x (* w target-y))))
            (if (and (/= elem 0)
                    (>= target-x 0) (< target-x w)
                    (>= target-y 0) (< target-y h)
                    (/= (@ grid-vector target-offset) elem))
              (with grid-vector target-offset elem)
              grid-vector)))))))

;; executes against game when drop-counter hits zero
(defun drop-block-one (game)
  (let* ((fb (@ game :fb))
         (dropped-fb (with fb :y (1+ (@ fb :y)))))
    (if (legal-block-position game dropped-fb)
      (with game :fb dropped-fb)
      (-> game
          ((lambda (g) (with g :main-grid
                             (grid-combine-fun
                               (@ g :main-grid) (falling-block-grid fb)
                               (@ fb :x) (@ fb :y)))))
          (with :fb nil)))))

;; player moves piece
(defun move-piece (game x y)
  (let ((fb (@ game :fb)))
    (if fb
      (let ((new-fb (-> fb
                      (with :x (+ (@ fb :x) x))
                      (with :y (+ (@ fb :y) y)))))
        (if (legal-block-position game new-fb)
          (with game :fb new-fb)
          game))
      game)))

;; player rotates piece
(defun rotate-piece (game n)
  (let ((fb (@ game :fb)))
    (if fb
      (let ((rotated-fb (rotate-falling-block fb n)))
        (if (legal-block-position game rotated-fb)
          (with game :fb rotated-fb)
          game))
      game)))

;; player drops the block
(defun drop-block (game)
  (if (@ game :fb)
    (drop-block (drop-block-one game))
    game))

(defun do-action (game action)
  (case action
    (:left (move-piece game -1 0))
    (:right (move-piece game 1 0))
    (:down (move-piece game 0 1))
    (:r-left (rotate-piece game -1))
    (:r-right (rotate-piece game 1))
    (:drop (drop-block game))
    (otherwise game)))

(defun partition (seq n &key (result (fset:seq)))
  (if (= 0 (size seq))
    result
    (partition (subseq seq n) n :result (with-last result (subseq seq 0 n)))))

(defun row-full (seq)
  (every (lambda (x) (/= x 0)) (convert 'list seq)))

(defun add-empty-rows (rows n width)
  (if (> n 0)
    (add-empty-rows
      (with-first rows
                  (convert 'seq (loop for i upto (1- width) collecting 0)))
      (1- n) width)
    rows))

(defun clear-lines (game)
  (let* ((main-grid (@ game :main-grid))
         (width (@ main-grid :width))
         (rows (partition (@ main-grid :v) width))
         (filtered-rows (filter (complement #'row-full) rows))
         (cleared-lines (- (@ main-grid :height) (size filtered-rows)))
         (all-rows (add-empty-rows filtered-rows cleared-lines width))
         (new-vec (reduce #'concat all-rows)))
    (-> game
        (with :main-grid (with main-grid :v new-vec))
        (change-key :cleared-lines (lambda (x) (+ x cleared-lines))))))

(defun inc-level (game)
  (with game :level (1+ (truncate (@ game :cleared-lines) 10))))

(defun update-keys-down (game incoming-keys-down)
  (with game :keys-down
    (let ((old-keys-down (with-default (@ game :keys-down) 0)))
      (gmap :map
            (lambda (key)
              (values key (1+ (@ old-keys-down key))))
            (:set incoming-keys-down)))))

(defun do-actions-aux (game keys-down-alist)
  (if keys-down-alist
    (destructuring-bind (key . n-frames-down) (car keys-down-alist)
      (-> game
          ((lambda (g)
             (if (or (= 1 n-frames-down)
                     (and (>= n-frames-down *key-delay-frames*)
                          (= 0 (mod (- n-frames-down *key-delay-frames*)
                                    *key-repeat-frames*))))
               (do-action g key)
               g)))
          (do-actions-aux (cdr keys-down-alist))))
    game))

(defun do-actions (game)
  (do-actions-aux game (convert 'list (@ game :keys-down))))

(defun create-piece-if-none (game)
  (if (null (@ game :fb))
    (multiple-value-bind
      (updated-game new-fb) (make-falling-block game :x 3 :y 0)
      (-> updated-game
          (with :fb new-fb)
          drop-counter-reset))
    game))

(defun drop-if-time (game)
  (if (<= (@ game :drop-counter) 0)
    (-> game
        drop-block-one
        drop-counter-reset)
    game))

(defun change-key (game key fn &rest extra)
  (with game key (apply fn (@ game key) extra)))

;; a pure function, takes a game hash and a set of keys pressed
;; returns an updated game hash
(defun step-game (game keys-down)
  (if (@ game :active)
    (-> game
        (update-keys-down keys-down)
        do-actions
        clear-lines
        inc-level
        create-piece-if-none
        drop-if-time
        (change-key :drop-counter #'1-)
        (change-key :frame-counter #'1+))
    game))

(defmethod contains? ((m fset:map) x)
  (multiple-value-bind (v found) (@ m x)
    (declare (ignore v))
    found))

;; imperative game loop
(defun game-loop ()
  (let ((game (make-game))
        (keys-down #{ })
        (reverse-history '()))
    (sdl:with-init ()
      (sdl:initialise-default-font)
      (sdl:window 320 240 :title-caption "block game")
      (setf (sdl:frame-rate) 60)

      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key key)
          (when (sdl:key= key :sdl-key-escape)
            (sdl:push-quit-event))
          (when (sdl:key= key :sdl-key-r)
            (setf *game-mode* :reverse))
          (when (sdl:key= key :sdl-key-f)
            (setf *game-mode* :playing))
          (when (sdl:key= key :sdl-key-p)
            (setf *game-mode* :pause))
          (when (contains? *key-map* key)
            (setf keys-down (with keys-down (@ *key-map* key))))
          )
        (:key-up-event (:key key)
          (when (contains? *key-map* key)
            (setf keys-down (less keys-down (@ *key-map* key))))
          )
        (:idle ()
          ;; step game forward
          (cond
            ((eq *game-mode* :playing)
             (progn
               (push game reverse-history)
               (setf game (step-game game keys-down))
               ))
            ((eq *game-mode* :reverse)
             (progn
               (when reverse-history
                 (setf game (pop reverse-history))))))
          ;; Clear the display each game loop
          (sdl:clear-display sdl:*black*)
          ;; Draw game
          (draw-game game 10 10 10 10)
          ;; Redraw the display
          (sdl:update-display))))))

