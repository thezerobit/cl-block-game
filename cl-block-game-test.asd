#|
  This file is a part of cl-block-game project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-block-game-test-asd
  (:use :cl :asdf))
(in-package :cl-block-game-test-asd)

(defsystem cl-block-game-test
  :author "Stephen A. Goss"
  :license "Modified BSD"
  :depends-on (:cl-block-game
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-block-game"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
