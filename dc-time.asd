(asdf:defsystem :dc-time
  :description "Time functions"
  :author "Donnie Cameron <macnod@gmail.com>"
  :licence "MIT License"
  :depends-on (:dc-ds)
  :serial t
  :components ((:file "dc-time-package")
                (:file "dc-time")))
