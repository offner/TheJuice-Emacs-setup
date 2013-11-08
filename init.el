(require 'cl)

;; Load Marmalade Package Manager
(require 'package)
(setq package-archives
      '(("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Package List
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit-eshell auto-complete paredit
  idle-highlight-mode find-file-in-project smex ido-ubiquitous magit yasnippet
   solarized-theme exec-path-from-shell flymake-jslint flymake-cursor go-mode
   ipython magit speedbar)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Startup Directories
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'load-path "~/.emacs.d/vendor/gocode/")
(add-to-list 'load-path "~/.emacs.d/vendor/gocode/emacs/")
(add-to-list 'load-path "~/.emacs.d/vendor/python-mode.el-6.1.1/")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(when (memq window-system '(mac ns))
  ;; Setup Path
  (exec-path-from-shell-initialize)
  (set-frame-font "Monaco-16"))

;; el-get
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)
(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files/")

;; Auto Complete
;; Setup before language sepcific sections
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-start 2)
(setq ac-ignore-case nil)
(setq ac-sources (append ac-sources '(ac-source-go)))
(define-key ac-complete-mode-map "\C-n" 'ac-next) ;;keybind
(define-key ac-complete-mode-map "\C-p" 'ac-previous) ;;keybind

(require 'magit)
;; JS Dev settings : Load improved js2-mode : https://github.com/mooz/js2-mode
(require 'js-settings)
;; Python Dev
(require 'python-settings)
;; Column Marker @ 80
(require 'column-marker)
(mapc (lambda (hook)
        (add-hook hook (lambda () (interactive) (column-marker-1 80))))
      '(org-mode-hook
        emacs-lisp-mode-hook
        python-mode-hook
        js2-mode-hook
        text-mode-hook))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "117284df029007a8012cae1f01c3156d54a0de4b9f2f381feab47809b8a1caef" "5debeb813b180bd1c3756306cd8c83ac60fda55f85fb27249a0f2d55817e3cab" default)))
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(py-tab-indent nil))


;; emacs-nav http://code.google.com/p/emacs-nav
;;(require 'nav)

;; Org Mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ;;not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link);;keybind
(global-set-key "\C-ca" 'org-agenda);;keybind
(global-set-key "\C-cb" 'org-iswitchb);;keybind

;; go
(require 'go-mode-load)
(require 'go-autocomplete)
(eval-after-load "go-mode"
  '(progn
		 (setq c-indent-level 2)
     (require 'auto-complete-config)))

(defun my-go-mode-hook ()
  (setq tab-width 4 indent-tabs-mode 1))
(add-hook 'go-mode-hook 'my-go-mode-hook)



;; YASnippet
(require 'yasnippet)
(yas/initialize)
;; Load the snippet files themselves
(yas/load-directory "~/.emacs.d/elpa/yasnippet-0.6.1/snippets/text-mode")
;; Let's have snippets in the auto-complete dropdown
(add-to-list 'ac-sources 'ac-source-yasnippet)

;; Set nxml-mode for .config files
(add-to-list 'auto-mode-alist '("\\.config\\'" . nxml-mode))

;; Mustache Mode! https://github.com/mustache/emacs
(require 'mustache-mode)

;; Spell Check - Aspell
;;(setq-default ispell-program-name "C:\\Program Files (x86)\\Aspell\\bin\\aspell.exe")
(setq text-mode-hook '(lambda()
                        (flyspell-mode t)       ; spellchek (sic) on the fly
                        ))

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

;; General Settings
(desktop-save-mode 1)
(delete-selection-mode 1)
(global-hi-lock-mode 1)
(setq ring-bell-function 'ignore) ;; disable visible bell
;; Show whitespace, remove on save
(setq-default show-trailing-whitespace 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode 1)
(setq linum-format "%3d")
(setq whitespace-line-column nil) ;; Disables annoying 80 column font-lock in starter kit

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :background "#fdf6e3" :foreground "#657b83" :weight thin :height 0.75)))))

;; Colorize the shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Smooth scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
