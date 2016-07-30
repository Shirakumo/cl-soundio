(ql:quickload '(cl-soundio verbose))
(defpackage #:cl-soundio-example
  (:use #:cl)
  (:export #:main))
(in-package #:cl-soundio-example)

(defvar *offset* 0.0f0)

(defmacro with-err (form datum &rest args)
  (let ((err (gensym "ERR")))
    `(let ((,err ,form))
       (when (/= 0 ,err)
         (error ,datum ,@args (cl-soundio-cffi:strerror ,err))))))

(defmacro with-ctor ((var ctor dtor) &body body)
  `(let ((,var ,ctor))
     (unwind-protect
          (progn
            (when (cffi:null-pointer-p ,var)
              (error "Failed to create ~a" ',var))
            ,@body)
       ,dtor)))

(cl-soundio-cffi:define-callback-func writer ((stream :pointer) (min :int) (max :int))
  (let* ((layout (cffi:foreign-slot-pointer stream 'cl-soundio-cffi:outstream 'cl-soundio-cffi::layout))
         (sample-rate (cl-soundio-cffi:outstream-sample-rate stream))
         (seconds/frame (/ 1.0f0 sample-rate))
         (frames-left max))
    (v:info :test "WRITER")
    (cffi:with-foreign-object (frame-count :int)
      (cffi:with-foreign-object (areas :pointer)
        (loop while (< 0 frames-left)
              do (setf (cffi:mem-ref frame-count :int) frames-left)
                 (with-err (cl-soundio-cffi:outstream-begin-write stream areas frame-count)
                   "Failed to begin write: ~a")
                 (when (= 0 (cffi:mem-ref frame-count :int))
                   (return))
                 (let* ((pitch 440.0f0)
                        (rads (* pitch 2.0f0 PI)))
                   (loop for frame from 0 below (cffi:mem-ref frame-count :int)
                         for sample = (sin (* rads (+ *offset* (* frame seconds/frame))))
                         do (loop for channel from 0 below (cl-soundio-cffi:channel-layout-channel-count layout)
                                  for area = (cffi:foreign-aref (cffi:mem-ref areas :pointer) '(:struct cl-soundio-cffi:channel-area) channel)
                                  for ptr = (cl-soundio-cffi:channel-area-ptr area)
                                  for step = (cl-soundio-cffi:channel-area-step area)
                                  do (setf (cffi:mem-ref (cffi:make-pointer (+ ptr (* step frame))) :float) sample))))
                 (incf *offset* (* seconds/frame (cffi:mem-ref frame-count :int)))
                 (with-err (cl-soundio-cffi:outstream-end-write stream)
                   "Failed to end write: ~a")
                 (decf frames-left))))))

(defun main ()
  (with-ctor (soundio (cl-soundio-cffi:create)
                      (cl-soundio-cffi:destroy soundio))
    (with-err (cl-soundio-cffi:connect soundio)
      "Failed to connect: ~a")
    
    (cl-soundio-cffi:flush-events soundio)

    (let ((default-device-index (cl-soundio-cffi:default-output-device-index soundio)))
      (when (< default-device-index 0)
        (error "No output device found."))

      (with-ctor (device (cl-soundio-cffi:get-output-device soundio default-device-index)
                         (cl-soundio-cffi:device-unref device))
        (format T "~&Found output device: ~a" (cl-soundio-cffi:device-name device))

        (with-ctor (stream (cl-soundio-cffi:outstream-create device)
                           (cl-soundio-cffi:outstream-destroy stream))
          (setf (cl-soundio-cffi:outstream-format stream) cl-soundio-cffi:format-float32-be)
          (setf (cl-soundio-cffi:outstream-write-callback stream) (cffi:callback writer))

          (with-err (cl-soundio-cffi:outstream-open stream)
            "Unable to open device: ~a")

          (with-err (cl-soundio-cffi:outstream-layout-error stream)
            "Unable to set channel layout: ~a")

          (with-err (cl-soundio-cffi:outstream-start stream)
            "Unable to start device: ~a")

          ;; (loop for i from 0
          ;;       do (print i)
          ;;          (cl-soundio-cffi:wait-events soundio))
          )))))
