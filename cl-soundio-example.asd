(asdf:defsystem cl-soundio-example
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "The ported soundio sine example."
  :homepage "https://shirakumo.org/docs/cl-soundio/"
  :bug-tracker "https://shirakumo.org/project/cl-soundio/issues"
  :source-control (:git "https://shirakumo.org/project/cl-soundio.git")
  :serial T
  :components ((:file "example"))
  :depends-on (:cl-soundio
               :verbose))
