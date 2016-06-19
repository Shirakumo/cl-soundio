#|
 This file is a part of cl-soundio
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:cl-soundio-cffi
  (:nicknames #:org.shirakumo.fraf.soundio.cffi)
  (:use #:cl #:cffi)
  ;; low-level.lisp
  (:export
   #:*static*
   #:libsoundio))

(defpackage #:cl-soundio
  (:nicknames #:org.shirakumo.fraf.soundio)
  (:use #:cl #:org.shirakumo.fraf.soundio.cffi)
  ;; wrapper.lisp
  (:export
   ))
