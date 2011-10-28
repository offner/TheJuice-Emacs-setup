(require 'cl)
;; Set file for customize
(setq custom-file "~/.emacs.d/customizations.el")
     (load custom-file)

;; Load Marmalade Package Manager
(require 'package)
(setq package-archives
      '(("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Package List
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-eshell auto-complete)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Startup Directories
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendor/")

;; Theme / Font
(add-to-list 'load-path "~/.emacs.d/themes/solarized/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized/")
(set-frame-font "Ubuntu Mono-12")
(load-theme 'solarized-dark)

;; JS Dev settings : Load improved js2-mode : https://github.com/mooz/js2-mode
(require 'js-settings)

;; emacs-nav http://code.google.com/p/emacs-nav
(require 'nav)

;; Org Mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ;;not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link);;keybind
(global-set-key "\C-ca" 'org-agenda);;keybind
(global-set-key "\C-cb" 'org-iswitchb);;keybind


;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-start 2)
(setq ac-ignore-case nil)
(define-key ac-complete-mode-map "\C-n" 'ac-next) ;;keybind
(define-key ac-complete-mode-map "\C-p" 'ac-previous) ;;keybind

;; XML Mode TODO:NXML
(add-to-list 'auto-mode-alist '("\\.config\\'" . xml-mode))

;; Mustache Mode! https://github.com/mustache/emacs
(require 'mustache-mode)

;; Spell Check - Aspell
(setq-default ispell-program-name "C:\\Program Files (x86)\\Aspell\\bin\\aspell.exe")
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
;; Show whitespace, remove on save
(setq-default show-trailing-whitespace 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode 1)
(setq whitespace-line-column nil) ;; Disables annoying 80 column font-lock in starter kit