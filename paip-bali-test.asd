#|
  This file is a part of paip-bali project.
  Copyright (c) 2018 balisun
|#

(in-package :cl-user)
(defpackage paip-bali-test-asd
  (:use :cl :asdf))
(in-package :paip-bali-test-asd)

(defsystem paip-bali-test
  :author "balisun"
  :license ""
  :depends-on (:paip-bali
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "paip-bali"))))
  :description "Test system for paip-bali"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
