#|
 This file is a part of cl-soundio
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.soundio.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(define-foreign-library libsoundio
  (:darwin (:or "libsoundio.dylib" "libsoundio.so"
                #+X86 "mac32-libsoundio.dylib"
                #+X86-64 "mac64-libsoundio.dylib"))
  (:unix (:or "libsoundio.so"
              #+X86 "lin32-libsoundio.so"
              #+X86-64 "lin64-libsoundio.so"))
  (:windows (:or "soundio.dll"
                 #+X86 "win32-libsoundio.dll"
                 #+X86-64 "win64-libsoundio.dll"))
  (t (:default "soundio")))

(use-foreign-library libsoundio)
