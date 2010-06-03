; -*- mode: lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; begin iwp .emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons "~/dotfiles" load-path))
(autoload 'doc-mode "doc-mode")

(require 'git)

(defun my-c-mode-common-hook ()
  (setq tab-width 4 indent-tabs-mode nil)
  (define-key c-mode-map "\C-m" 'newline-and-indent))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq auto-mode-alist
      (append auto-mode-alist
	      '(
		("\\.[hg]s$"  . haskell-mode)
		("\\.doc$"    . doc-mode)
		("\\.hi$"     . haskell-mode)
		("\\.hsc$"    . haskell-mode)
		("\\.l[hg]s$" . literate-haskell-mode)
		("\\.rhtml$"  . html-mode)
		)))

(global-set-key [f3] 'manual-entry)
(global-set-key [f4] 'dabbrev-expand)
(global-set-key [f5] 'kill-this-buffer)
(global-set-key [f6] 'buffer-menu)
(global-set-key [f7] 'font-lock-fontify-buffer)
(global-set-key [f8] 'indent-region)
(global-set-key [f9] 'next-error)
(global-set-key [f10] 'compile)

;; timeclock stuff
(require 'timeclock)
(require 'timeclock-janrain)
(setq timeclock-query-project-interval (* 15 60))
(timeclock-modeline-display 1)
(remove-hook 'timeclock-out-hook 'timeclock-query-comment)
(setq timeclock-workday (* 60 60 12))
(timeclock-query-in)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end iwp .emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of .emacs hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
