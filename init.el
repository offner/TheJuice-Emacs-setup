(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Package List
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(moe-theme)
  "A list of packages to install at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; General Settings
(setq mac-command-modifier 'super)
(setq mac-right-option-modifier 'meta)
(setq mac-option-key-is-meta t)
(setq mac-option-modifier 'meta)
(desktop-save-mode 1)
(delete-selection-mode 1)
(global-hi-lock-mode 1)
(setq ring-bell-function 'ignore) ;; disable visible bell
;; Show whitespace, remove on save
(setq-default show-trailing-whitespace 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;(global-linum-mode 1)
;;(setq linum-format "%3d ")
(setq whitespace-line-column nil) ;; Disables annoying 80 column font-lock in starter kit
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Custom stuff set through UI
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (moe-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#303030" :foreground "#c6c6c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "Input"))))
 '(line-number ((t (:background "#4e4e4e" :foreground "#b2b2b2" :weight light :height 90))))
 '(line-number-current-line ((t (:background "#d7ff5f" :foreground "#3a3a3a" :height 90)))))


(require 'moe-theme)
(moe-dark)
(moe-theme-set-color 'orange)



;; JS Notes
;; tide + induim
