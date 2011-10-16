(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(setq js2-consistent-level-indent-inner-bracket-p t)
(setq js2-basic-offset 2)
(setq js2-cleanup-whitespace t)
;;(add-hook 'js2-mode-hook 'turn-on-font-lock)
;;(setq js2-use-font-lock-faces t)
(font-lock-add-keywords
 'js2-mode `(("\\(function *\\)("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1), "\u0192")
                        nil)))))
(add-hook 'js2-mode-hook 'esk-paredit-nonlisp)
(add-hook 'js2-mode-hook
          (lambda ()
            (define-key js2-mode-map "\C-cc" 'comment-or-uncomment-region)
            (define-key js2-mode-map "{" 'paredit-open-curly)
            (define-key js2-mode-map "}" 'paredit-close-curly-and-newline)
            (define-key js2-mode-map (kbd ",") 'self-insert-command)
            )
          )

(provide 'js-settings)
