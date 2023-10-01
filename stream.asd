(defsystem "stream"
  :version "0.0.1"
  :author "Yi Cao"
  :mailto "me@ycao.top"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "stream/tests"))))

(defsystem "stream/tests"
  :author "Yi Cao"
  :license "MIT"
  :depends-on ("stream"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for stream"
  :perform (test-op (op c) (symbol-call :rove :run c)))
