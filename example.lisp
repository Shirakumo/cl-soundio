(defpackage #:cl-soundio-example
  (:use #:cl)
  (:export #:main)
  (:local-nicknames (:soundio :cl-soundio-cffi)))
(in-package #:cl-soundio-example)

(defvar seconds-offset 0.0f0)
(declaim (type single-float seconds-offset))

(defconstant INT32_MAX #x7fffffff)
(defconstant INT32_MIN (- (- INT32_MAX) 1))
(defconstant INT16_MAX #x7fff)
(defconstant INT16_MIN (- (- INT16_MAX) 1))

(defmacro with-err (form datum &rest args)
  (let ((err (gensym "ERR")))
    `(let ((,err ,form))
       (declare (type fixnum ,err))
       (when (/= 0 ,err)
         (error ,datum ,@args (soundio:strerror ,err))))))

(defmacro with-ctor ((var ctor dtor) &body body)
  `(let ((,var ,ctor))
     (unwind-protect
          (progn
            (when (cffi:null-pointer-p ,var)
              (error "Failed to create ~a" ',var))
            ,@body)
       ,dtor)))

(defun write-sample-s16-be (ptr sample)
  (let ((range (- (float INT16_MAX 0.0d0) (float INT16_MIN 0.0d0))))
    (setf (cffi:mem-ref ptr :int16) (* sample range 0.5))))

(defun write-sample-s32-be (ptr sample)
  (let ((range (- (float INT32_MAX 0.0d0) (float INT32_MIN 0.0d0))))
    (setf (cffi:mem-ref ptr :int32) (* sample range 0.5))))

(declaim (inline write-sample-float32-be))
(defun write-sample-float32-be (ptr sample)
  (declare (optimize speed))
  (setf (cffi:mem-ref ptr :float) (float (the single-float sample) 0.0s0)))

(declaim (inline write-sample-float64-be))
(defun write-sample-float64-be (ptr sample)
  (declare (optimize speed))
  (setf (cffi:mem-ref ptr :double) sample))

(declaim (inline write-callback))
(defun write-callback (outstream frame-count-min frame-count-max)
  (declare (ignore frame-count-min))
  (declare (optimize speed))
  (declare (type fixnum frame-count-min frame-count-max))
  (let* ((float-sample-rate (soundio:outstream-sample-rate outstream))
         (seconds-per-frame (/ 1.0s0 float-sample-rate))
         (frames-left frame-count-max))
    (declare (type fixnum float-sample-rate frames-left))
    (cffi:with-foreign-object (areas :pointer)
      (loop (cffi:with-foreign-object (frame-count :int)
              (setf (cffi:mem-ref frame-count :int) frames-left)
              
              (with-err (soundio:outstream-begin-write outstream areas frame-count)
                "Unable to begin write: ~a")
              (let ((frame-count (cffi:mem-ref frame-count :int))
                    (areas (cffi:mem-ref areas :pointer)))
                (declare (type fixnum frame-count))

                (when (= 0 frame-count) (return))

                (let* ((layout (cffi:foreign-slot-pointer outstream '(:struct soundio:outstream) 'soundio::layout))
                       (pitch 440.0s0)
                       (radians-per-second (* pitch 2.0s0 (float PI 0.0s0))))
                  (loop for frame of-type fixnum from 0 below frame-count
                        for sample = (sin (* (+ seconds-offset (* frame seconds-per-frame))
                                             radians-per-second))
                        do (loop for channel of-type fixnum from 0 below (soundio:channel-layout-channel-count layout)
                                 for area = (cffi:mem-aptr areas '(:struct soundio:channel-area) channel)
                                 for area-ptr = (soundio:channel-area-ptr area)
                                 for area-step = (soundio:channel-area-step area)
                                 do (write-sample-float32-be area-ptr sample)
                                    (cffi:incf-pointer area-ptr area-step))))
                (incf seconds-offset (* seconds-per-frame frame-count))

                (with-err (soundio:outstream-end-write outstream)
                  "Unable to end write: ~a")

                (setf frames-left (the fixnum (- frames-left frame-count)))
                (when (<= frames-left 0)
                  (return))))))))

(cffi:defcallback write-callback :void ((outstream :pointer) (frame-count-min :int) (frame-count-max :int))
  (write-callback outstream frame-count-min frame-count-max))

(cffi:defcallback underflow-callback :void ((oustream :pointer))
  (format T "~&WARN: Underflow~%"))

(defun main ()
  (setf *exit* NIL)
  (cffi:with-foreign-string (stream-name "cl-soundio-example")
    (let ((latency 0.0d0)
          (sample-rate 0))
      
      (with-ctor (soundio (soundio:create)
                          (soundio:destroy soundio))
        (with-err (soundio:connect-backend soundio soundio:backend-pulse-audio)
          "Failed to connect: ~a")

        (format T "~&Backend: ~a" (soundio:backend-name (soundio:soundio-current-backend soundio)))
        
        (soundio:flush-events soundio)

        (let ((default-device-index (soundio:default-output-device-index soundio)))
          (when (< default-device-index 0)
            (error "No output device found."))

          (with-ctor (device (soundio:get-output-device soundio default-device-index)
                             (soundio:device-unref device))
            (format T "~&Output device: ~a" (soundio:device-name device))

            (with-err (soundio:device-probe-error device)
              "Cannot probe device: ~a")

            (with-ctor (stream (soundio:outstream-create device)
                               (soundio:outstream-destroy stream))
              (setf (soundio:outstream-write-callback stream) (cffi:callback write-callback))
              (setf (soundio:outstream-name stream) stream-name)
              (setf (soundio:outstream-software-latency stream) latency)
              (setf (soundio:outstream-sample-rate stream) sample-rate)

              (cond ((soundio:device-supports-format device soundio:format-float32-be)
                     (setf (fdefinition 'write-sample) #'write-sample-float32-be)
                     (setf (soundio:outstream-format stream) soundio:format-float32-be))
                    ((soundio:device-supports-format device soundio:format-float64-be)
                     (setf (fdefinition 'write-sample) #'write-sample-float64-be)
                     (setf (soundio:outstream-format stream) soundio:format-float64-be))
                    ((soundio:device-supports-format device soundio:format-s32-be)
                     (setf (fdefinition 'write-sample) #'write-sample-s32-be)
                     (setf (soundio:outstream-format stream) soundio:format-s32-be))
                    ((soundio:device-supports-format device soundio:format-s16-be)
                     (setf (fdefinition 'write-sample) #'write-sample-s16-be)
                     (setf (soundio:outstream-format stream) soundio:format-s16-be))
                    (T
                     (error "No suitable device format available.")))

              (with-err (soundio:outstream-open stream)
                "Unable to open device: ~a")

              (format T "~&Software latency: ~a" (soundio:outstream-software-latency stream))

              (with-err (soundio:outstream-layout-error stream)
                "Unable to set channel layout: ~a")

              (with-err (soundio:outstream-start stream)
                "Unable to start device: ~a")

              (loop (soundio:flush-events soundio)))))))))
