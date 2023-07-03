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
  error-none
  error-no-mem
  error-init-audio-backend
  error-system-resources
  error-opening-device
  error-no-such-device
  error-invalid
  error-backend-unavailable
  error-streaming
  error-incompatible-device
  error-no-such-client
  error-incompatible-backend
  error-backend-disconnected
  error-interrupted
  error-underflow
  error-encoding-string)

(defcenum channel-id
  channel-id-invalid
  channel-id-front-left
  channel-id-front-right
  channel-id-front-center
  channel-id-lfe
  channel-id-back-left
  channel-id-back-right
  channel-id-front-left-center
  channel-id-front-right-center
  channel-id-back-center
  channel-id-side-left
  channel-id-side-right
  channel-id-top-center
  channel-id-top-front-left
  channel-id-top-front-center
  channel-id-top-front-right
  channel-id-top-back-left
  channel-id-top-back-center
  channel-id-top-back-right
  channel-id-back-left-center
  channel-id-back-right-center
  channel-id-front-left-wide
  channel-id-front-right-wide
  channel-id-front-left-high
  channel-id-front-center-high
  channel-id-front-right-high
  channel-id-top-front-left-center
  channel-id-top-front-right-center
  channel-id-top-side-left
  channel-id-top-side-right
  channel-id-left-lfe
  channel-id-right-lfe
  channel-id-lfe2
  channel-id-bottom-center
  channel-id-bottom-left-center
  channel-id-bottom-right-center
  channel-id-ms-mid
  channel-id-ms-side
  channel-id-ambisonic-w
  channel-id-ambisonic-x
  channel-id-ambisonic-y
  channel-id-ambisonic-z
  channel-id-xy-x
  channel-id-xy-y
  channel-id-headphones-left
  channel-id-headphones-right
  channel-id-click-track
  channel-id-foreign-language
  channel-id-hearing-impaired
  channel-id-narration
  channel-id-haptic
  channel-id-dialog-centric-mix
  channel-id-aux
  channel-id-aux0
  channel-id-aux1
  channel-id-aux2
  channel-id-aux3
  channel-id-aux4
  channel-id-aux5
  channel-id-aux6
  channel-id-aux7
  channel-id-aux8
  channel-id-aux9
  channel-id-aux10
  channel-id-aux11
  channel-id-aux12
  channel-id-aux13
  channel-id-aux14
  channel-id-aux15)

(defcenum channel-layout-id
  channel-layout-id-mono
  channel-layout-id-stereo
  channel-layout-id2-point1
  channel-layout-id3-point0
  channel-layout-id3-point0-back
  channel-layout-id3-point1
  channel-layout-id4-point0
  channel-layout-id-quad
  channel-layout-id-quad-side
  channel-layout-id4-point1
  channel-layout-id5-point0-back
  channel-layout-id5-point0-side
  channel-layout-id5-point1
  channel-layout-id5-point1-back
  channel-layout-id6-point0-side
  channel-layout-id6-point0-front
  channel-layout-id-hexagonal
  channel-layout-id6-point1
  channel-layout-id6-point1-back
  channel-layout-id6-point1-front
  channel-layout-id7-point0
  channel-layout-id7-point0-front
  channel-layout-id7-point1
  channel-layout-id7-point1-wide
  channel-layout-id7-point1-wide-back
  channel-layout-id-octagonal)

(defcenum backend
  backend-none
  backend-jack
  backend-pulse-audio
  backend-alsa
  backend-core-audio
  backend-wasapi
  backend-dummy)

(defcenum device-aim
  device-aim-input 
  device-aim-output)

(defcenum format
  format-invalid
  format-s8
  format-u8
  format-s16-le
  format-s16-be
  format-u16-le
  format-u16-be
  format-s24-le
  format-s24-be
  format-u24-le
  format-u24-be
  format-s32-le
  format-s32-be
  format-u32-le
  format-u32-be
  format-float32-le
  format-float32-be
  format-float64-le
  format-float64-be)

(defconstant max-channels 24)

(defcstruct (channel-layout :class channel-layout :conc-name channel-layout-)
  (name :string)
  (channel-count :int)
  (channels :int :count #.max-channels))

(defcstruct (sample-rate-change :class sample-rate-change :conc-name sample-rate-change-)
  (min :int)
  (max :int))

(defcstruct (channel-area :class channel-area :conc-name channel-area-)
  (ptr :pointer)
  (step :int))

(defcstruct (soundio :class soundio :conc-name soundio-)
  (userdata :pointer)
  (on-devices-change :pointer)
  (on-backend-disconnect :pointer)
  (on-events-signal :pointer)
  (current-backend :int)
  (app-name :string)
  (emit-rtprio-warning :pointer)
  (jack-info-callback :pointer)
  (jack-error-callback :pointer))

(defcstruct (device :class device :conc-name device-)
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

(defcstruct (outstream :class outstream :conc-name outstream-)
  (device :pointer)
  (format :int)
  (sample-rate :int)
  (layout (:struct channel-layout))
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

(defcstruct (instream :class instream :conc-name instream-)
  (device :pointer)
  (format :int)
  (sample-rate :int)
  (layout (:struct channel-layout))
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

(defcstruct (ring-buffer :class ring-buffer :conc-name ring-buffer-))

(defmacro define-callback-func (name args &body body)
  `(defcallback ,name :void ,args     
     (ignore-errors
      (with-simple-restart (abort "Abort the callback.")
        (handler-bind ((error #'invoke-debugger))
          ,@body)))))

(defcfun (version-string "soundio_version_string") :string)

(defcfun (version-major "soundio_version_major") :int)

(defcfun (version-minor "soundio_version_minor") :int)

(defcfun (version-patch "soundio_version_patch") :int)

(defcfun (create "soundio_create") :pointer)

(defcfun (destroy "soundio_destroy") :void
  (soundio :pointer))

(defcfun (connect "soundio_connect") :int
  (soundio :pointer))

(defcfun (connect-backend "soundio_connect_backend") :int
  (soundio :pointer)
  (backend :int))

(defcfun (disconnect "soundio_disconnect") :void
  (soundio :pointer))

(defcfun (strerror "soundio_strerror") :string
  (error :int))

(defcfun (backend-name "soundio_backend_name") :string
  (backend :int))

(defcfun (backend-count "soundio_backend_count") :int
  (soundio :pointer))

(defcfun (get-backend "soundio_get_backend") :int
  (soundio :pointer)
  (index :int))

(defcfun (have-backend "soundio_have_backend") :bool
  (backend :int))

(defcfun (flush-events "soundio_flush_events") :void
  (soundio :pointer))

(defcfun (wait-events "soundio_wait_events") :void
  (soundio :pointer))

(defcfun (wakeup "soundio_wakeup") :void
  (soundio :pointer))

(defcfun (force-device-scan "soundio_force_device_scan") :void
  (soundio :pointer))

(defcfun (channel-layout-equal "soundio_channel_layout_equal") :bool
  (a :pointer)
  (b :pointer))

(defcfun (get-channel-name "soundio_get_channel_name") :string
  (id :int))

(defcfun (parse-channel-id "soundio_parse_channel_id") :int
  (str :string)
  (str-len :int))

(defcfun (channel-layout-builtin-count "soundio_channel_layout_builtin_count") :int)

(defcfun (channel-layout-get-builtin "soundio_channel_layout_get_builtin") :pointer
  (index :int))

(defcfun (channel-layout-get-default "soundio_channel_layout_get_default") :pointer
  (channel-count :int))

(defcfun (channel-layout-find-channel "soundio_channel_layout_find_channel") :int
  (layout :pointer)
  (channel :int))

(defcfun (channel-layout-detect-builtin "soundio_channel_layout_detect_builtin") :bool
  (layout :pointer))

(defcfun (best-matching-channel-layout "soundio_best_matching_channel_layout") :pointer
  (preferred-layouts :pointer)
  (preferred-layout-count :int)
  (available-layouts :pointer)
  (available-layout-count :int))

(defcfun (sort-channel-layouts "soundio_sort_channel_layouts") :void
  (layouts :pointer)
  (layout-count :int))

(defcfun (get-bytes-per-sample "soundio_get_bytes_per_sample") :int
  (format :int))

(defcfun (format-string "soundio_format_string") :string
  (format :int))

(defcfun (input-device-count "soundio_input_device_count") :int
  (soundio :pointer))

(defcfun (output-device-count "soundio_output_device_count") :int
  (soundio :pointer))

(defcfun (get-input-device "soundio_get_input_device") :pointer
  (soundio :pointer)
  (index :int))

(defcfun (get-output-device "soundio_get_output_device") :pointer
  (soundio :pointer)
  (index :int))

(defcfun (default-input-device-index "soundio_default_input_device_index") :int
  (soundio :pointer))

(defcfun (default-output-device-index "soundio_default_output_device_index") :int
  (soundio :pointer))

(defcfun (device-ref "soundio_device_ref") :void
  (device :pointer))

(defcfun (device-unref "soundio_device_unref") :void
  (device :pointer))

(defcfun (device-equal "soundio_device_equal") :bool
  (a :pointer)
  (b :pointer))

(defcfun (device-sort-channel-layouts "soundio_device_sort_channel_layouts") :void
  (device :pointer))

(defcfun (device-supports-format "soundio_device_supports_format") :bool
  (device :pointer)
  (format :int))

(defcfun (device-supports-layout "soundio_device_supports_layout") :bool
  (device :pointer)
  (layout :pointer))

(defcfun (device-supports-sample-rate "soundio_device_supports_sample_rate") :bool
  (device :pointer)
  (sample-rate :int))

(defcfun (device-nearest-sample-rate "soundio_device_nearest_sample_rate") :int
  (device :pointer)
  (sample-rate :int))

(defcfun (outstream-create "soundio_outstream_create") :pointer
  (device :pointer))

(defcfun (outstream-destroy "soundio_outstream_destroy") :void
  (outstream :pointer))

(defcfun (outstream-open "soundio_outstream_open") :int
  (outstream :pointer))

(defcfun (outstream-start "soundio_outstream_start") :int
  (outstream :pointer))

(defcfun (outstream-begin-write "soundio_outstream_begin_write") :int
  (outstream :pointer)
  (areas :pointer)
  (frame-count (:pointer :int)))

(defcfun (outstream-end-write "soundio_outstream_end_write") :int
  (outstream :pointer))

(defcfun (outstream-clear-buffer "soundio_outstream_clear_buffer") :int
  (outstream :pointer))

(defcfun (outstream-pause "soundio_outstream_pause") :int
  (outstream :pointer)
  (pause :bool))

(defcfun (outstream-get-latency "soundio_outstream_get_latency") :int
  (outstream :pointer)
  (out-latency (:pointer :double)))

(defcfun (instream-create "soundio_instream_create") :pointer
  (device :pointer))

(defcfun (instream-destroy "soundio_instream_destroy") :void
  (instream :pointer))

(defcfun (instream-open "soundio_instream_open") :int
  (instream :pointer))

(defcfun (instream-start "soundio_instream_start") :int
  (instream :pointer))

(defcfun (instream-begin-read "soundio_instream_begin_read") :int
  (instream :pointer)
  (areas :pointer)
  (frame-count (:pointer :int)))

(defcfun (instream-end-read "soundio_instream_end_read") :int
  (instream :pointer))

(defcfun (instream-pause "soundio_instream_pause") :int
  (instream :pointer)
  (pause :bool))

(defcfun (instream-get-latency "soundio_instream_get_latency") :int
  (instream :pointer)
  (out-latency (:pointer :double)))

(defcfun (ring-buffer-create "soundio_ring_buffer_create") :pointer
  (soundio :pointer)
  (requested-capacity :int))

(defcfun (ring-buffer-destroy "soundio_ring_buffer_destroy") :void
  (ring-buffer :pointer))

(defcfun (ring-buffer-capacity "soundio_ring_buffer_capacity") :int
  (ring-buffer :pointer))

(defcfun (ring-buffer-write-ptr "soundio_ring_buffer_write_ptr") :string
  (ring-buffer :pointer))

(defcfun (ring-buffer-advance-write-ptr "soundio_ring_buffer_advance_write_ptr") :void
  (ring-buffer :pointer)
  (count :int))

(defcfun (ring-buffer-read-ptr "soundio_ring_buffer_read_ptr") :string
  (ring-buffer :pointer))

(defcfun (ring-buffer-advance-read-ptr "soundio_ring_buffer_advance_read_ptr") :void
  (ring-buffer :pointer)
  (count :int))

(defcfun (ring-buffer-fill-count "soundio_ring_buffer_fill_count") :int
  (ring-buffer :pointer))

(defcfun (ring-buffer-free-count "soundio_ring_buffer_free_count") :int
  (ring-buffer :pointer))

(defcfun (ring-buffer-clear "soundio_ring_buffer_clear") :void
  (ring-buffer :pointer))

(declaim (inline get-bytes-per-frame))
(defun get-bytes-per-frame (format channel-count)
  (* (get-bytes-per-sample format) channel-count))

(declaim (inline get-bytes-per-second))
(defun get-bytes-per-second (format channel-count sample-rate)
  (* (get-bytes-per-frame format channel-count) sample-rate))
