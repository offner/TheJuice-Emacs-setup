;;(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(eval-after-load 'js
  '(progn (define-key js-mode-map "{" 'paredit-open-curly)
          (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
          (define-key js-mode-map (kbd ",") 'self-insert-command)
          (define-key js-mode-map "\C-cc" 'comment-or-uncomment-region)
          (add-hook 'js-mode-hook 'esk-paredit-nonlisp)
          (setq js-indent-level 2)
))

;; Lintnode setup
(add-to-list 'load-path "~/Repositories/lintnode")
;;(require 'flymake-jslint)
(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)
;; Make sure we can find the lintnode executable
(setq lintnode-location "~/Repositories/lintnode")
(setq lintnode-autostart 1)
;; JSLint can be... opinionated
(setq lintnode-jslint-excludes (list 'nomen 'plusplus 'onevar 'white))
;; Start the server when we first open a js file and start checking
(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)
            (lintnode-hook)))



(provide 'js-settings)
