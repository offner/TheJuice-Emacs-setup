(require 'python)
(require 'python-mode)
(require 'ipython)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import i?pdb")
  (highlight-lines-matching-regexp "i?pdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)
;; Set python to tabs, for you Anthony!
(add-hook 'python-mode-hook
	  (lambda ()
                (setq indent-tabs-mode t)
		(setq python-indent 4)
                (setq tab-width 4)))

;; Initialize Pymacs
;;(setq py-load-pymacs nil)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;; Initialize Rope
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
(add-hook 'python-mode-hook 'jedi:setup)

(provide 'python-settings)
