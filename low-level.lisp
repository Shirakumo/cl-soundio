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

(defcenum soundio-error
  soundio-error-none
  soundio-error-no-mem
  soundio-error-init-audio-backend
  soundio-error-system-resources
  soundio-error-opening-device
  soundio-error-no-such-device
  soundio-error-invalid
  soundio-error-backend-unavailable
  soundio-error-streaming
  soundio-error-incompatible-device
  soundio-error-no-such-client
  soundio-error-incompatible-backend
  soundio-error-backend-disconnected
  soundio-error-interrupted
  soundio-error-underflow
  soundio-error-encoding-string)

(defcenum soundio-channel-id
  soundio-channel-id-invalid
  soundio-channel-id-front-left
  soundio-channel-id-front-right
  soundio-channel-id-front-center
  soundio-channel-id-lfe
  soundio-channel-id-back-left
  soundio-channel-id-back-right
  soundio-channel-id-front-left-center
  soundio-channel-id-front-right-center
  soundio-channel-id-back-center
  soundio-channel-id-side-left
  soundio-channel-id-side-right
  soundio-channel-id-top-center
  soundio-channel-id-top-front-left
  soundio-channel-id-top-front-center
  soundio-channel-id-top-front-right
  soundio-channel-id-top-back-left
  soundio-channel-id-top-back-center
  soundio-channel-id-top-back-right
  soundio-channel-id-back-left-center
  soundio-channel-id-back-right-center
  soundio-channel-id-front-left-wide
  soundio-channel-id-front-right-wide
  soundio-channel-id-front-left-high
  soundio-channel-id-front-center-high
  soundio-channel-id-front-right-high
  soundio-channel-id-top-front-left-center
  soundio-channel-id-top-front-right-center
  soundio-channel-id-top-side-left
  soundio-channel-id-top-side-right
  soundio-channel-id-left-lfe
  soundio-channel-id-right-lfe
  soundio-channel-id-lfe2
  soundio-channel-id-bottom-center
  soundio-channel-id-bottom-left-center
  soundio-channel-id-bottom-right-center
  soundio-channel-id-ms-mid
  soundio-channel-id-ms-side
  soundio-channel-id-ambisonic-w
  soundio-channel-id-ambisonic-x
  soundio-channel-id-ambisonic-y
  soundio-channel-id-ambisonic-z
  soundio-channel-id-xy-x
  soundio-channel-id-xy-y
  soundio-channel-id-headphones-left
  soundio-channel-id-headphones-right
  soundio-channel-id-click-track
  soundio-channel-id-foreign-language
  soundio-channel-id-hearing-impaired
  soundio-channel-id-narration
  soundio-channel-id-haptic
  soundio-channel-id-dialog-centric-mix
  soundio-channel-id-aux
  soundio-channel-id-aux0
  soundio-channel-id-aux1
  soundio-channel-id-aux2
  soundio-channel-id-aux3
  soundio-channel-id-aux4
  soundio-channel-id-aux5
  soundio-channel-id-aux6
  soundio-channel-id-aux7
  soundio-channel-id-aux8
  soundio-channel-id-aux9
  soundio-channel-id-aux10
  soundio-channel-id-aux11
  soundio-channel-id-aux12
  soundio-channel-id-aux13
  soundio-channel-id-aux14
  soundio-channel-id-aux15)

(defcenum soundio-channel-layout-id
  soundio-channel-layout-id-mono
  soundio-channel-layout-id-stereo
  soundio-channel-layout-id2-point1
  soundio-channel-layout-id3-point0
  soundio-channel-layout-id3-point0-back
  soundio-channel-layout-id3-point1
  soundio-channel-layout-id4-point0
  soundio-channel-layout-id-quad
  soundio-channel-layout-id-quad-side
  soundio-channel-layout-id4-point1
  soundio-channel-layout-id5-point0-back
  soundio-channel-layout-id5-point0-side
  soundio-channel-layout-id5-point1
  soundio-channel-layout-id5-point1-back
  soundio-channel-layout-id6-point0-side
  soundio-channel-layout-id6-point0-front
  soundio-channel-layout-id-hexagonal
  soundio-channel-layout-id6-point1
  soundio-channel-layout-id6-point1-back
  soundio-channel-layout-id6-point1-front
  soundio-channel-layout-id7-point0
  soundio-channel-layout-id7-point0-front
  soundio-channel-layout-id7-point1
  soundio-channel-layout-id7-point1-wide
  soundio-channel-layout-id7-point1-wide-back
  soundio-channel-layout-id-octagonal)

(defcenum soundio-backend
  soundio-backend-none
  soundio-backend-jack
  soundio-backend-pulse-audio
  soundio-backend-alsa
  soundio-backend-core-audio
  soundio-backend-wasapi
  soundio-backend-dummy)

(defcenum soundio-device-aim
  soundio-device-aim-input 
  soundio-device-aim-output)

(defcenum soundio-format
  soundio-format-invalid
  soundio-format-s8       
  soundio-format-u8       
  soundio-format-s16-le    
  soundio-format-s16-be    
  soundio-format-u16-le    
  soundio-format-u16-be    
  soundio-format-s24-le    
  soundio-format-s24-be    
  soundio-format-u24-le    
  soundio-format-u24-be    
  soundio-format-s32-le    
  soundio-format-s32-be    
  soundio-format-u32-le    
  soundio-format-u32-be    
  soundio-format-float32-le
  soundio-format-float32-be
  soundio-format-float64-le
  soundio-format-float64-be)

(defconstant soundio-max-channels 24)

(defcstruct soundio-channel-layout
  (name :string)
  (channel-count :int)
  (channels :int :count #.soundio-max-channels))

(defcstruct soundio-sample-rate-range
  (min :int)
  (max :int))

(defcstruct soundio-channel-area
  (ptr :pointer)
  (step :int))

(defcstruct soundio
  (userdata :pointer)
  (on-devices-change :pointer)
  (on-backend-disconnect :pointer)
  (on-events-signal :pointer)
  (current-backend :int)
  (app-name :string)
  (emit-rtprio-warning :pointer)
  (jack-info-callback :pointer)
  (jack-error-callback :pointer))

(defcstruct soundio-device
  (soundio :pointer)
  (id :string)
  (name :string)
  (aim :int)
  (layouts :pointer)
  (layout-count :int)
  (current-layout :pointer)
  (formats :pointer)
  (format-count :int)
  (current-format :int)
  (sample-rates :pointer)
  (sample-rate-count :int)
  (sample-rate-current :int)
  (software-latency-min :double)
  (software-latency-max :double)
  (software-latency-current :double)
  (is-raw :bool)
  (ref-count :int)
  (probe-error :int))

(defcstruct soundio-out-stream
  (device :pointer)
  (format :int)
  (sample-rate :int)
  (layout (:struct soundio-channel-layout))
  (software-latency :double)
  (userdata :pointer)
  (write-callback :pointer)
  (underflow-callback :pointer)
  (error-callback :pointer)
  (name :string)
  (non-terminal-hint :bool)
  (bytes-per-frame :int)
  (bytes-per-sample :int)
  (layout-error :int))

(defcstruct soundio-in-stream
  (device :pointer)
  (format :int)
  (sample-rate :int)
  (layout (:struct soundio-channel-layout))
  (software-latency :double)
  (userdata :pointer)
  (read-callback :pointer)
  (overflow-callback :pointer)
  (error-callback :pointer)
  (name :string)
  (non-terminal-hint :bool)
  (bytes-per-frame :int)
  (bytes-per-sample :int)
  (layout-error :int))

(defcstruct soundio-ring-buffer)

(defcfun (soundio-version-string "soundio_version_string") :string)

(defcfun (soundio-version-major "soundio_version_major") :int)

(defcfun (soundio-version-minor "soundio_version_minor") :int)

(defcfun (soundio-version-patch "soundio_version_patch") :int)

(defcfun (soundio-create "soundio_create") :pointer)

(defcfun (soundio-destroy "soundio_destroy") :void
  (soundio :pointer))

(defcfun (soundio-connect "soundio_connect") :int
  (soundio :pointer))

(defcfun (soundio-connect-backend "soundio_connect_backend") :int
  (soundio :pointer)
  (backend :int))

(defcfun (soundio-disconnect "soundio_disconnect") :void
  (soundio :pointer))

(defcfun (soundio-strerror "soundio_strerror") :string
  (error :int))

(defcfun (soundio-backend-name "soundio_backend_name") :string
  (backend :int))

(defcfun (soundio-backend-count "soundio_backend_count") :int
  (soundio :pointer))

(defcfun (soundio-get-backend "soundio_get_backend") :int
  (soundio :pointer)
  (index :int))

(defcfun (soundio-have-backend "soundio_have_backend") :bool
  (backend :int))

(defcfun (soundio-flush-events "soundio_flush_events") :void
  (soundio :pointer))

(defcfun (soundio-wait-events "soundio_wait_events") :void
  (soundio :pointer))

(defcfun (soundio-wakeup "soundio_wakeup") :void
  (soundio :pointer))

(defcfun (soundio-force-device-scan "soundio_force_device_scan") :void
  (soundio :pointer))

(defcfun (soundio-channel-layout-equal "soundio_channel_layout_equal") :bool
  (a :pointer)
  (b :pointer))

(defcfun (soundio-get-channel-name "soundio_get_channel_name") :string
  (id :int))

(defcfun (soundio-parse-channel-id "soundio_parse_channel_id") :int
  (str :string)
  (str-len :int))

(defcfun (soundio-channel-layout-builtin-count "soundio_channel_layout_builtin_count") :int)

(defcfun (soundio-channel-layout-get-builtin "soundio_channel_layout_get_builtin") :pointer
  (index :int))

(defcfun (soundio-channel-layout-get-default "soundio_channel_layout_get_default") :pointer
  (channel-count :int))

(defcfun (soundio-channel-layout-find-channel "soundio_channel_layout_find_channel") :int
  (layout :pointer)
  (channel :int))

(defcfun (soundio-channel-layout-detect-builtin "soundio_channel_layout_detect_builtin") :bool
  (layout :pointer))

(defcfun (soundio-best-matching-channel-layout "soundio_best_matching_channel_layout") :pointer
  (preferred-layouts :pointer)
  (preferred-layout-count :int)
  (available-layouts :pointer)
  (available-layout-count :int))

(defcfun (soundio-sort-channel-layouts "soundio_sort_channel_layouts") :void
  (layouts :pointer)
  (layout-count :int))

(defcfun (soundio-get-bytes-per-sample "soundio_get_bytes_per_sample") :int
  (format :int))

(defcfun (soundio-format-string "soundio_format_string") :string
  (format :int))

(defcfun (soundio-input-device-count "soundio_input_device_count") :int
  (soundio :pointer))

(defcfun (soundio-output-device-count "soundio_output_device_count") :int
  (soundio :pointer))

(defcfun (soundio-get-input-device "soundio_get_input_device") :pointer
  (soundio :pointer)
  (index :int))

(defcfun (soundio-get-output-device "soundio_get_output_device") :pointer
  (soundio :pointer)
  (index :int))

(defcfun (soundio-default-input-device-index "soundio_default_input_device_index") :int
  (soundio :pointer))

(defcfun (soundio-default-output-device-index "soundio_default_output_device_index") :int
  (soundio :pointer))

(defcfun (soundio-device-ref "soundio_device_ref") :void
  (device :pointer))

(defcfun (soundio-device-unref "soundio_device_unref") :void
  (device :pointer))

(defcfun (soundio-device-equal "soundio_device_equal") :bool
  (a :pointer)
  (b :pointer))

(defcfun (soundio-device-sort-channel-layouts "soundio_device_sort_channel_layouts") :void
  (device :pointer))

(defcfun (soundio-device-supports-format "soundio_device_supports_format") :bool
  (device :pointer)
  (format :int))

(defcfun (soundio-device-supports-layout "soundio_device_supports_layout") :bool
  (device :pointer)
  (layout :pointer))

(defcfun (soundio-device-supports-sample-rate "soundio_device_supports_sample_rate") :bool
  (device :pointer)
  (sample-rate :int))

(defcfun (soundio-device-nearest-sample-rate "soundio_device_nearest_sample_rate") :int
  (device :pointer)
  (sample-rate :int))

(defcfun (soundio-outstream-create "soundio_outstream_create") :pointer
  (device :pointer))

(defcfun (soundio-outstream-destroy "soundio_outstream_destroy") :void
  (outstream :pointer))

(defcfun (soundio-outstream-open "soundio_outstream_open") :int
  (outstream :pointer))

(defcfun (soundio-outstream-start "soundio_outstream_start") :int
  (outstream :pointer))

(defcfun (soundio-outstream-begin-write "soundio_outstream_begin_write") :int
  (outstream :pointer)
  (areas :pointer)
  (frame-count (:pointer :int)))

(defcfun (soundio-outstream-end-write "soundio_outstream_end_write") :int
  (outstream :pointer))

(defcfun (soundio-outstream-clear-buffer "soundio_outstream_clear_buffer") :int
  (outstream :pointer))

(defcfun (soundio-outstream-pause "soundio_outstream_pause") :int
  (outstream :pointer)
  (pause :bool))

(defcfun (soundio-outstream-get-latency "soundio_outstream_get_latency") :int
  (outstream :pointer)
  (out-latency (:pointer :double)))

(defcfun (soundio-instream-create "soundio_instream_create") :pointer
  (device :pointer))

(defcfun (soundio-instream-destroy "soundio_instream_destroy") :void
  (instream :pointer))

(defcfun (soundio-instream-open "soundio_instream_open") :int
  (instream :pointer))

(defcfun (soundio-instream-start "soundio_instream_start") :int
  (instream :pointer))

(defcfun (soundio-instream-begin-read "soundio_instream_begin_read") :int
  (instream :pointer)
  (areas :pointer)
  (frame-count (:pointer :int)))

(defcfun (soundio-instream-end-read "soundio_instream_end_read") :int
  (instream :pointer))

(defcfun (soundio-instream-pause "soundio_instream_pause") :int
  (instream :pointer)
  (pause :bool))

(defcfun (soundio-instream-get-latency "soundio_instream_get_latency") :int
  (instream :pointer)
  (out-latency (:pointer :double)))

(defcfun (soundio-ring-buffer-create "soundio_ring_buffer_create") :pointer
  (soundio :pointer)
  (requested-capacity :int))

(defcfun (soundio-ring-buffer-destroy "soundio_ring_buffer_destroy") :void
  (ring-buffer :pointer))

(defcfun (soundio-ring-buffer-capacity "soundio_ring_buffer_capacity") :int
  (ring-buffer :pointer))

(defcfun (soundio-ring-buffer-write-ptr "soundio_ring_buffer_write_ptr") :string
  (ring-buffer :pointer))

(defcfun (soundio-ring-buffer-advance-write-ptr "soundio_ring_buffer_advance_write_ptr") :void
  (ring-buffer :pointer)
  (count :int))

(defcfun (soundio-ring-buffer-read-ptr "soundio_ring_buffer_read_ptr") :string
  (ring-buffer :pointer))

(defcfun (soundio-ring-buffer-advance-read-ptr "soundio_ring_buffer_advance_read_ptr") :void
  (ring-buffer :pointer)
  (count :int))

(defcfun (soundio-ring-buffer-fill-count "soundio_ring_buffer_fill_count") :int
  (ring-buffer :pointer))

(defcfun (soundio-ring-buffer-free-count "soundio_ring_buffer_free_count") :int
  (ring-buffer :pointer))

(defcfun (soundio-ring-buffer-clear "soundio_ring_buffer_clear") :void
  (ring-buffer :pointer))

(declaim (inline soundio-get-bytes-per-frame))
(defun soundio-get-bytes-per-frame (format channel-count)
  (* (soundio-get-bytes-per-sample format) channel-count))

(declaim (inline soundio-get-bytes-per-second))
(defun soundio-get-bytes-per-second (format channel-count sample-rate)
  (* (soundio-get-bytes-per-frame format channel-count) sample-rate))
