;;--------------;;
;; Early Extras ;;
;;--------------;;

(let* ((extras-dir (concat user-emacs-directory "early-extras"))
       (extras-files (ignore-errors (directory-files extras-dir t "^.*\\.el$"))))
  (dolist (file extras-files) (load file)))

;;--------------;;
;; Setup elpaca ;;
;;--------------;;

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;----------;;
;; Packages ;;
;;----------;;

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package smartparens
  :ensure t
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (lisp-mode . smartparens-strict-mode)
         (lisp-interaction-mode . smartparens-strict-mode)
         (ielm-mode . smartparens-strict-mode)
         (slime-mode . smartparens-strict-mode)
         (scheme-mode . smartparens-strict-mode))
  :config
  (smartparens-global-mode)
  (require 'smartparens-config)

  (define-key smartparens-strict-mode-map (kbd "C-<backspace>") 'sp-backspace)
  (define-key smartparens-strict-mode-map (kbd "C-<return>") 'sp-return)

  (evil-define-minor-mode-key 'normal 'smartparens-mode
    (kbd "C-.") 'sp-smart-slurp)
  (evil-define-minor-mode-key 'normal 'smartparens-mode
    (kbd "C-,") 'sp-forward-barf-sexp)
  (evil-define-minor-mode-key 'normal 'smartparens-mode
    (kbd ">") 'sp-smart-slurp)
  (evil-define-minor-mode-key 'normal 'smartparens-mode
    (kbd "<") 'sp-forward-barf-sexp) )

(use-package hungry-delete
  :ensure t
  :hook (smartparens-strict-mode . hungry-delete-mode)
  :config (setq hungry-delete-join-reluctantly t))

(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :demand t
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (define-key evil-window-map "w" 'other-frame)
  (define-key evil-window-map "b" 'xref-go-back)
  (define-key evil-motion-state-map "C-w b" nil))

(use-package evil-collection
  :ensure t
  :demand t
  :config (evil-collection-init))

(use-package evil-smartparens
  :ensure t
  :hook (smartparens-enabled . evil-smartparens-mode))

;; (if (display-graphic-p)
;;     (use-package corfu
;;       :ensure t
;;       :custom (corfu-auto t)
;;       :config
;;       (global-corfu-mode)
;;       (setq tab-always-indent 'complete))
;;   (use-package corfu-terminal
;;     :ensure t
;;     :custom (corfu-auto t)
;;     :config
;;     (corfu-terminal-mode +1)
;;     (global-corfu-mode)
;;     (setq tab-always-indent 'complete)))

(use-package savehist
  :config (savehist-mode))

(use-package vertico
  :ensure t
  :config (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :config (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-x C-b" . consult-buffer)
         ("C-x C-g" . consult-grep)))

(use-package swiper
  :ensure t
  :config (evil-define-key nil evil-normal-state-map (kbd "/") 'swiper))

(use-package symbol-overlay
  :ensure t
  :hook ((emacs-lisp-mode . symbol-overlay-mode)
         (lisp-mode . symbol-overlay-mode)
         (lisp-interaction-mode . symbol-overlay-mode)
         (ielm-mode . symbol-overlay-mode)
         (slime-mode . symbol-overlay-mode)))

(when (display-graphic-p)
  (use-package doom-modeline
    :ensure t
    :config
    (doom-modeline-mode 1)
    (line-number-mode)
    (column-number-mode)
    (setq doom-modeline-percent-position nil)
    (setq doom-modeline-position-column-line-format '("%l:%c"))))

(use-package modus-themes
  :ensure t)

(if (display-graphic-p)
    (use-package auto-dark
      :ensure t
      :config
      (setq auto-dark-dark-theme 'modus-vivendi-tinted
            auto-dark-light-theme 'modus-operandi-tinted)
      (auto-dark-mode))
  (load-theme 'modus-vivendi))

(when (display-graphic-p)
  (use-package vterm
    :ensure t))

;; (use-package sly
;;   :ensure t
;;   :config
;;   (define-key sly-mode-map (kbd "C-w d") 'sly-edit-definition)
;;   (define-key sly-mode-map (kbd "C-w b") 'sly-pop-find-definition-stack)
;;   (define-key sly-mode-map (kbd "C-s t") 'sly-toggle-trace-fdefinition)
;;   (setq sly-lisp-implementations
;;         '((sbcl ("sbcl") :coding-system utf-8-unix)
;;           (qlot ("qlot" "exec" "sbcl") :coding-system utf-8-unix))))

(use-package company
  :ensure t
  :config
  (global-company-mode 1))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-repl slime-scratch slime-trace-dialog slime-cl-indent slime-company)))

(use-package slime-company
  :after (slime company)
  :ensure t
  :config (setq slime-company-completion 'fuzzy))

(use-package clhs
  :ensure t)

(use-package icicles
  :ensure (:host github :repo "emacsmirror/icicles"))

(use-package web-mode
  :ensure t
  :hook ((html-mode . web-mode))
  :config (setq web-mode-markup-indent-offset 2))

(use-package gnuplot
  :ensure t)

(if (display-graphic-p)
    (use-package git-gutter-fringe
      :ensure t
      :config
      (git-gutter:start-update-timer)
      (global-git-gutter-mode 1))
  (use-package git-gutter
    :ensure t
    :config
    (git-gutter:start-update-timer)
    (global-git-gutter-mode 1)))

;;-----;;
;; Org ;;
;;-----;;

(use-package org
  :config
  (push 'org-tempo org-modules)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((lisp . t) (gnuplot . t)))
  (setq org-babel-lisp-eval-fn 'slime-eval)
  (setq org-image-actual-width 420)
  (setq org-startup-with-inline-images t)
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook
            #'(lambda ()
                (org-display-inline-images)
                (org-redisplay-inline-images))))

(use-package org-sliced-images
  :ensure t
  :config (org-sliced-images-mode))

;;------------;;
;; Formatting ;;
;;------------;;

(defvar *auto-format* nil)

(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

(add-hook 'before-save-hook #'(lambda () (when *auto-format* (format-buffer))))

;;------------;;
;; Appearance ;;
;;------------;;

;; get lambda symbol and stuff
(global-prettify-symbols-mode)

;; slim down GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 0)

;; big and pretty font
(set-face-attribute 'default nil :height 140)
(set-face-attribute 'default nil :font "SF Mono-16")

;;------;;
;; Misc ;;
;;------;;

;; (setq debug-on-error t)

;; get rid of plugin warnings
(setq warning-minimum-level :emergency)

;; fancy scrolling
(setq scroll-conservatively 101)
(setq scroll-margin 3)

;; overflowing long lines
(setq-default truncate-lines t)

;; too many direds
(setq dired-kill-when-opening-new-dired-buffer t)

(global-auto-revert-mode 1)

;;----------------;;
;; Andrzej Indent ;;
;;----------------;;

;; (setq lisp-indent-function 'sly-common-lisp-indent-function)

;; (sly-define-common-lisp-style "more-modern"
;;  "A good general purpose style based on modern but less dogmatic."
;;  (:inherit "modern")
;;  (:variables
;;   (sly-lisp-loop-body-forms-indentation 0) ; <~ was AAF
;;   (sly-lisp-loop-indent-subclauses t))) ; <~ reenable

;; (setq sly-common-lisp-style-default "more-modern")

;;----------;;
;; Commands ;;
;;----------;;

(defun close-saved-buffers ()
  "Utility to clean up all the buffers I accumulate."
  (interactive)
  (cl-flet ((savedp (buf)
              (when-let ((name (buffer-file-name buf)))
                (and (file-exists-p name)
                     (not (buffer-modified-p buf))))))
    (cl-mapcar #'kill-buffer (cl-remove-if-not #'savedp (buffer-list)))))

(defun untabify-buffer ()
  "Untabify the whole buffer."
  (save-excursion (untabify (point-min) (point-max))))

(defun toggle-auto-format ()
  "Toggles *auto-format*, this effects formatting on save and in smartparens."
  (interactive)
  (message "auto-format set to %S." (setq *auto-format* (not *auto-format*))))

(defun config ()
  "Open init.el"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun indent-buffer ()
  "Indent and untabify the whole buffer."
  (interactive)
  (unless (eq major-mode 'yaml-mode)
    (save-excursion
      (indent-region (point-min) (point-max)))))

(defun format-buffer ()
  "Remove trailing space, indent, and untabify buffer."
  (interactive)
  (cond ((member major-mode '(lisp-mode emacs-lisp-mode c-mode slime-mrepl-mode))
         (indent-buffer)
         (untabify-buffer)
         (delete-trailing-whitespace))
        (t nil)))

(defun increase-font-size ()
  "Increase font size by 10 units."
  (interactive)
  (let ((h (+ (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height h)))

(defun decrease-font-size ()
  "Decrease font size by 10 units."
  (interactive)
  (let ((h (- (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height h)))

;;--------------;;
;; Lisp Editing ;;
;;--------------;;

(defun sp-wants-space-after-p ()
  (not (or (looking-at "\\s-")
           (looking-at ")")
           (looking-at "]")
           (looking-at "}"))))

(defun sp-wants-space-before-p ()
  (not (or (char-equal ?\( (char-before (point)))
           (char-equal ?\{ (char-before (point)))
           (char-equal ?\) (char-after (point)))
           (char-equal ?\} (char-after (point))))))

(defun sp-smart-slurp ()
  "Forward slurp and try to insert space."
  (interactive)
  (sp-forward-slurp-sexp)
  (skip-syntax-forward "w_")
  (when (sp-wants-space-after-p)
    (insert " ")))

(defun sp-return ()
  "Insert newline and format if *auto-format*."
  (interactive)
  (newline)
  (when (eq major-mode 'slime-mrepl-mode)
    (slime-mrepl-indent-and-complete-symbol nil))
  (when *auto-format* (format-buffer))
  (skip-chars-forward " \t"))

(defun sp-backspace ()
  "Delete preceding whitespace and format if *auto-format*."
  (interactive)
  (delete-trailing-whitespace)
  (if (looking-back "^[ \t]+" (line-beginning-position))
      (progn (delete-horizontal-space)
             (delete-backward-char 1)
             (when (sp-wants-space-before-p) (insert " ")))
    (delete-backward-char 1))
  (when *auto-format* (format-buffer)))

;;-------------;;
;; Keybindings ;;
;;-------------;;

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)

;; (setf ns-command-modifier 'meta)

(cl-defmacro define-keys (keymap &body bindings)
  `(progn ,@(mapcar
             (lambda (binding)
               `(define-key ,keymap
                            ,(car binding)
                            ,(cadr binding)))
             bindings)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defvar custom-emulation-keymap
  (let ((map (make-sparse-keymap)))
    (define-keys
     map
     ((kbd "C-=") 'increase-font-size)
     ((kbd "C--") 'decrease-font-size)
     ((kbd "C-h") 'windmove-left)
     ((kbd "C-j") 'windmove-down)
     ((kbd "C-k") 'windmove-up)
     ((kbd "C-l") 'windmove-right)
     ((kbd "C-w b") 'xref-go-back)
     ((kbd "C-c C-q") 'indent-buffer)
     ((kbd "M-l q") 'slime-quit-lisp)
     ((kbd "M-l s") 'slime)
     ((kbd "M-l v") 'vterm)
     ((kbd "M-l t") 'ansi-term)
     ((kbd "M-l e") 'eshell)
     ((kbd "M-l m") 'ielm))
    map))

(defvar custom-keymap
  (let ((map (make-sparse-keymap)))
    (define-keys
     map
     ((kbd "C-w d") 'evil-goto-definition)
     )
    map))

(define-minor-mode custom-emulation-mode
  "Custom emulation keybindings."
  :global t :keymap custom-emulation-keymap)

(add-to-list 'emulation-mode-map-alists
             `((custom-emulation-mode . ,custom-emulation-keymap)))

(define-minor-mode custom-mode
  "Custom keybindings."
  :global t :keymap custom-keymap)

;;--------;;
;; Extras ;;
;;--------;;

(let* ((extras-dir (concat user-emacs-directory "extras"))
       (extras-files (ignore-errors (directory-files extras-dir t "^.*\\.el$"))))
  (dolist (file extras-files) (load file)))

;;-------------;;
;; Auto Config ;;
;;-------------;;

(custom-emulation-mode)
(custom-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fea8f3bd01d0f4e63dc0e6eddd699753e1b6abcd7ec4c9e4fe55efbb2d3f04d5" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
