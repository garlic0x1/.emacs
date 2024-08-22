(defun hypr-clients ()
  (json-read-from-string
   (shell-command-to-string "hyprctl clients -j")))

(defun timestamp ()
  (apply #'concat (cl-mapcar #'number-to-string (current-time))))

(defun prompt-frame-opts ()
  `((name . "eprompt")
    (minibuffer . only)
    (fullscreen . 0)
    (undecorated . t)
    (auto-raise . t)
    ;; (internal-border-width . 10)
    ;; (width . 80)
    ;; (height . 200)
    ))

(cl-defmacro with-prompt-frame ((handler) &body body)
  (declare (indent defun))
  `(let ((frame (make-frame (prompt-frame-opts))))
     ;; (while (not (frame-visible-p frame)))
     (minibuffer-with-setup-hook 'minibuffer-complete
       (let ((res nil))
         (unwind-protect (setq res (progn ,@body))
           (delete-frame frame)
           (sit-for 0.1)
           (when ,handler
             (funcall ,handler res))))))
  ;; (with-selected-frame (make-frame (prompt-frame-opts))
  ;;    (sit-for 0.2)
  ;;    (frame-focus)
  ;;    (let ((res nil))
  ;;      (unwind-protect (setq res (progn ,@body))
  ;;        (delete-frame)
  ;;        (sit-for 0.1)
  ;;        (when ,handler
  ;;          (funcall ,handler res)))))
  )

(defun bar-frame-opts ()
  `((name . "ebar")
    (fullscreen . 0)
    (undecorated . t)
    (auto-raise . t)
    (height . 1)))

(defun eprompt-alist (message alist)
  (message "%s" alist)
  (let ((choice (completing-read message alist)))
    (cdr (assoc choice alist))))

(defun ekillprscrn ()
  (interactive)
  (call-process-shell-command "pkill wl-screenrec")
  (message "Stopped recording."))

(defun eprintscreen ()
  (interactive)
  (with-prompt-frame (#'call-process-shell-command)
    (let* ((filename (concat "~/Screencasts/wl-screenrec-"
                             (timestamp)
                             ".mp4"))
           (fstring (eprompt-alist
                     "Print Screen: "
                     (list (cons "Video"
                                 (concat "wl-screenrec %s -f "
                                         filename
                                         " && wl-copy < "
                                         filename
                                         " &"))
                           (cons "Image"
                                 "grim %s - | wl-copy -t image/png &"))))
           (area (eprompt-alist
                  "Area: "
                  '(("All" . "")
                    ("Selection" . "-g \"$(slurp -d)\"")))))
      (format fstring area))))

(defun elauncher ()
  "https://gitlab.com/dwt1/configuring-emacs/-/blob/main/07-the-final-touches/scripts/app-launchers.el?ref_type=heads#L28"
  (interactive)
  (with-selected-frame (make-frame (prompt-frame-opts))
    (message "elauncher")
    (sit-for 0.2)
    (frame-focus)
    (unwind-protect (app-launcher-run-app t)
      (delete-frame))))

(defun ewindows ()
  (interactive)
  (with-prompt-frame ((lambda (address)
                        (shell-command
                         (concat "hyprctl dispatch focuswindow address:"
                                 address)
                         nil nil)))
    (eprompt-alist
     "Window: "
     (cl-mapcar (lambda (client)
                  (cons (cdr (assoc 'class client))
                        (cdr (assoc 'address client))))
                (hypr-clients)))))

(defun eplayer ()
  (interactive)
  (with-prompt-frame ((lambda (cmd)
                        (shell-command
                         (concat "playerctl " cmd)
                         nil nil)))
    (completing-read "Player: " '("play-pause"
                                  "play"
                                  "pause"
                                  "stop"
                                  "next"
                                  "previous"))))

(defun ebar ()
  (interactive)
  (with-selected-frame (make-frame (bar-frame-opts))
    (sit-for 1)
    (insert "Hello")
    (sit-for 5)
    (delete-frame)))
