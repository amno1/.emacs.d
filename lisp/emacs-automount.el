;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'dbus)

(cl-flet (( udisks-register-signal (&rest args)
            (apply 'dbus-register-signal
                   :system "org.freedesktop.UDisks"
                   "/org/freedesktop/UDisks"
                   "org.freedesktop.UDisks"
                   args))
          ( udisks-call-method (&rest args)
            (apply 'dbus-call-method
                   :system "org.freedesktop.UDisks"
                   (car args)
                   "org.freedesktop.UDisks.Device"
                   (cdr args)))
          ( udisks-get-property (&rest args)
            (apply 'dbus-get-property
                   :system "org.freedesktop.UDisks"
                   (car args)
                   "org.freedesktop.UDisks.Device"
                   (cdr args))))
  (udisks-register-signal
   "DeviceAdded"
   (lambda (disk-path)
     (let (mount-point)
       (and (udisks-get-property disk-path "DeviceIsPartition")
            (equal (udisks-get-property disk-path "IdUsage")
                   "filesystem")
            (not (udisks-get-property disk-path "DeviceIsOpticalDisc"))
            (setq mount-point
                  (udisks-call-method disk-path
                                      "FilesystemMount"
                                      (udisks-get-property disk-path "IdType")
                                      '(:array)))
            (message "Mounted to: %s" mount-point)))
     )))

(provide 'emacs-automount)