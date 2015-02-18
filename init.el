(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync 'projectile 'helm 'auto-complete 'paredit 'exec-path-from-shell
	'scala-mode2 'column-marker)

;; Mac Specific
(when (memq window-system '(mac ns))
  ;; Setup Path
  (exec-path-from-shell-initialize)
  (set-frame-font "Monaco-16"))

;; Helm
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "s-p") 'helm-imenu)
(helm-mode 1)
(helm-autoresize-mode 1)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)

;; Org Mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ;;not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link);;keybind
(global-set-key "\C-ca" 'org-agenda);;keybind
(global-set-key "\C-cb" 'org-iswitchb);;keybind

(require 'column-marker)
(column-marker-1 80)
(mapc (lambda (hook)
        (add-hook hook (lambda () (interactive) (column-marker-1 80))))
      '(org-mode-hook
        emacs-lisp-mode-hook
        python-mode-hook
        js2-mode-hook
        text-mode-hook
	go-mode-hook))

;; General Settings
(setq backup-directory-alist '(("." . "~/.emacs.d/.emacs-backups")))
(desktop-save-mode 1)
(delete-selection-mode 1)
(global-hi-lock-mode 1)
(setq ring-bell-function 'ignore) ;; disable visible bell
;; Show whitespace, remove on save
(setq-default show-trailing-whitespace 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Smooth scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

;; Backup Settings
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message file)
      (delete-file file))))
