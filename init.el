;;--------------;;
;; Setup elpaca ;;
;;--------------;;

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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

(use-package smartparens
  :ensure t
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (lisp-mode . smartparens-strict-mode)
         (lisp-interaction-mode . smartparens-strict-mode)
         (ielm-mode . smartparens-strict-mode)
         (sly-mode . smartparens-strict-mode)
         (scheme-mode . smartparens-strict-mode))
  :config
  (smartparens-global-mode)
  (require 'smartparens-config)

  (define-key smartparens-strict-mode-map (kbd "<backspace>") 'sp-backspace)
  (define-key smartparens-strict-mode-map (kbd "C-<return>") 'sp-return)

  (evil-define-minor-mode-key 'normal 'smartparens-mode
    (kbd "C-.") 'sp-smart-slurp)

  (evil-define-minor-mode-key 'normal 'smartparens-mode
    (kbd "C-,") 'sp-forward-barf-sexp))

(use-package evil
  :ensure t
  :demand t
  :config
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :ensure t
  :demand t
  :config (evil-collection-init))

(use-package evil-smartparens
  :ensure t
  :hook (smartparens-enabled . evil-smartparens-mode))

(use-package corfu
  :ensure t
  :custom (corfu-auto t)
  :config
  (global-corfu-mode)
  (setq tab-always-indent 'complete))

(use-package eglot
  :config
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'javascript-mode-hook 'eglot-ensure))

(use-package savehist
  :config (savehist-mode))

(use-package vertico
  :ensure t
  :config (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :config (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-x C-b" . consult-buffer)
         ("C-x C-g" . consult-ripgrep)))

(use-package ivy
  :ensure t
  :config
  ;; (ivy-mode)
  )

(use-package swiper
  :ensure t
  :config (evil-define-key nil evil-normal-state-map (kbd "/") 'swiper))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  (line-number-mode)
  (column-number-mode)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-position-column-line-format '("%l:%c")))

(use-package auto-dark
  :ensure t
  :config
  (setq auto-dark-dark-theme 'modus-vivendi)
  (setq auto-dark-light-theme 'modus-operandi)
  (auto-dark-mode))

(use-package vterm
  :ensure t)

(use-package sly
  :ensure t
  :config
  (define-key sly-mode-map (kbd "C-w d") 'sly-edit-definition)
  (define-key sly-mode-map (kbd "C-w b") 'sly-pop-find-definition-stack)
  (setq sly-lisp-implementations
        '((sbcl ("sbcl") :coding-system utf-8-unix)
          (qlot ("qlot" "exec" "sbcl") :coding-system utf-8-unix))))

;; (use-package slime
;;   :ensure t
;;   :config
;;   (setq slime-lisp-implementations
;;      '((sbcl ("sbcl") :coding-system utf-8-unix)
;;           (qlot ("qlot" "exec" "sbcl") :coding-system utf-8-unix))))

;; (use-package coalton-mode
;;   :ensure (:host github :repo "coalton-lang/coalton-mode"))

(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package apprentice
  :ensure (:host github :repo "sasanidas/apprentice"))

(use-package web-mode
  :ensure t
  :hook ((html-mode . web-mode))
  :config (setq web-mode-markup-indent-offset 2))

(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable))

;;------------;;
;; Formatting ;;
;;------------;;

(defvar *auto-format* t)

(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

(add-hook 'before-save-hook
          '(lambda ()
             (when *auto-format*
               (delete-trailing-whitespace)
               (indent-buffer))))

;;------------;;
;; Appearance ;;
;;------------;;

;; get lambda symbol and stuff
(global-prettify-symbols-mode)

;; slim down GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode)

;; big letters
(set-face-attribute 'default nil :height 140)

;;------;;
;; Misc ;;
;;------;;

(setq debug-on-error t)

;; get rid of plugin warnings
(setq warning-minimum-level :emergency)

;; fancy scrolling
(setq scroll-conservatively 101)
(setq scroll-margin 3)

;;----------;;
;; Commands ;;
;;----------;;

(defun untabify-buffer ()
  "Untabify the whole buffer."
  (save-excursion (untabify (point-min) (point-max))))

(defun toggle-auto-format ()
  (interactive)
  (message "auto-format set to %S." (setq *auto-format* (not *auto-format*))))

(defun config ()
  "Open this file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun indent-buffer ()
  "Indent and untabify the whole buffer."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max))))

(defun format-buffer ()
  "Remove trailing space, indent, and untabify buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun increase-font-size ()
  (interactive)
  (let ((h (+ (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height h)))

(defun decrease-font-size ()
  (interactive)
  (let ((h (- (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height h)))

;;--------------;;
;; Lisp Editing ;;
;;--------------;;

(defun sp-smart-slurp ()
  (interactive)
  (sp-forward-slurp-sexp)
  (skip-syntax-forward "w_")
  (when (sp-wants-space-after-p)
    (insert " ")))

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

(defun sp-back-space ()
  (when (looking-back "^[ \t]+" (line-beginning-position))
    (delete-horizontal-space)
    t))

(defun sp-return ()
  (interactive)
  (newline)
  (when *auto-format* (format-buffer))
  (skip-chars-forward " \t"))

(defun sp-backspace ()
  (interactive)
  (delete-trailing-whitespace)
  (if (sp-back-space)
      (progn (delete-backward-char 1)
             (when (sp-wants-space-before-p) (insert " ")))
    (delete-backward-char 1))
  (when *auto-format* (indent-buffer)))

;;-------------;;
;; Keybindings ;;
;;-------------;;

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
     ((kbd "C-w b") 'evil-jump-backward)
     ((kbd "C-c C-q") 'indent-buffer)
     ((kbd "M-l s") 'sly)
     ((kbd "M-l c") 'cider-jack-in-clj)
     ((kbd "M-l v") 'vterm)
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

(custom-emulation-mode)
(custom-mode)
