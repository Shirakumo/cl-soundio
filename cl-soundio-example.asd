#|
 This file is a part of cl-soundio
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem cl-soundio-example
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "The ported soundio sine example."
  :homepage "https://Shirakumo.github.io/cl-soundio/"
  :bug-tracker "https://github.com/Shirakumo/cl-soundio/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-soundio.git")
  :serial T
  :components ((:file "example"))
  :depends-on (:cl-soundio
               :verbose))
