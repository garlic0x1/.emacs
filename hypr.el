(defun prompt-frame-opts ()
  `((name . "eprompt")
    (minibuffer . only)
    (fullscreen . 0)
    (undecorated . t)
    (auto-raise . t)
    (internal-border-width . 10)
    (width . 80)
    (height . 200)))

(defun eprompt-alist (message alist)
  (let ((choice (consult--read alist :prompt message)))
    (cdr (assoc choice alist))))

(defun ekillprscrn ()
  (interactive)
  (call-process-shell-command "pkill wl-screenrec")
  (message "Stopped recording."))

(defun eprintscreen ()
  (interactive)
  (with-selected-frame (make-frame (prompt-frame-opts))
    (sit-for 0.2)
    (let ((cmd nil))
      (unwind-protect
          (let* ((action (eprompt-alist
                          "Print Screen: "
                          '(("Video" . "wl-screenrec -f ~/Screencasts/rec.mp4")
                            ("Image" . "grim")
                            ("Audio/Video" . "wl-screenrec --audio -f ~/Screencasts/rec.mp4"))))
                 (area (eprompt-alist
                        "Area: "
                        '(("All" . "")
                          ("Selection" . " -g \"$(slurp -d)\""))))
                 (suffix
                  (if (equal action "Image")
                      " - | wl-copy -t image/png &"
                    " &")))
            (setq cmd (concat action area suffix)))
        (progn
          (delete-frame)
          (sit-for 0.1)
          (call-process-shell-command cmd ))))))

(defun elauncher ()
  "https://gitlab.com/dwt1/configuring-emacs/-/blob/main/07-the-final-touches/scripts/app-launchers.el?ref_type=heads#L28"
  (interactive)
  (with-selected-frame (make-frame (prompt-frame-opts))
    (sit-for 0.2)
    (unwind-protect (app-launcher-run-app t)
      (delete-frame))))

;; hack that doesnt work
;; (run-at-time 0.1 nil
;;              (lambda ()
;;                (frame-focus)
;;                (end-of-buffer)
;;                (insert " ")
;;                (backward-delete-char 1)))
