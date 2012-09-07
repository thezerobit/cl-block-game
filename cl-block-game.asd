#|
  This file is a part of cl-block-game project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

#|
  Author: Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-block-game-asd
  (:use :cl :asdf))
(in-package :cl-block-game-asd)

(defsystem cl-block-game
  :version "0.1"
  :author "Stephen A. Goss"
  :license "Modified BSD"
  :depends-on (:fset
               :lispbuilder-sdl)
  :components ((:module "src"
                :components
                ((:file "cl-block-game"))))
  :description "A Typical Falling Blocks Game"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op cl-block-game-test))))
