; -*- mode: lisp; -*-

(setq load-path (cons "~/dotfiles" load-path))

(require 'git)
;; (require 'timeclock)
;; (require 'timeclock-janrain)

(autoload 'doc-mode "doc-mode")
(autoload 'javascript-mode "javascript" nil t)
(autoload 'graphviz-dot-mode "graphviz-dot-mode")

(setq line-move-visual nil)
(setq column-number-mode t)
(put 'narrow-to-region 'disabled nil)

;; file extensions
(setq auto-mode-alist
      (append auto-mode-alist
	      '(
		("\\.[hg]s$"  . haskell-mode)
		("\\.doc$"    . doc-mode)
		("\\.dot$"    . graphviz-dot-mode)
		("\\.hi$"     . haskell-mode)
		("\\.hsc$"    . haskell-mode)
		("\\.js$"     . javascript-mode)
		("\\.l[hg]s$" . literate-haskell-mode)
		("\\.erb$"  . html-mode)
		("\\.rhtml$"  . html-mode)
		)))

(setq completion-ignored-extensions
      (append completion-ignored-extensions
	      '(
		".d"
		".x"
		)))

;; c-mode
(defun my-c-mode-common-hook ()
  (setq tab-width 2 indent-tabs-mode nil)
  (define-key c-mode-map "\C-m" 'newline-and-indent))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; function keys
(global-set-key [f3] 'manual-entry)
(global-set-key [f4] 'dabbrev-expand)
(global-set-key [f5] 'kill-this-buffer)
(global-set-key [f6] 'buffer-menu)
(global-set-key [f7] 'font-lock-fontify-buffer)
(global-set-key [f8] 'indent-region)
(global-set-key [f9] 'next-error)
(global-set-key [f10] 'compile)

;; timeclock
;; (timeclock-modeline-display 1)
;; (timeclock-query-project-off)
;; (remove-hook 'timeclock-out-hook 'timeclock-query-comment)
;; (setq timeclock-workday (* 60 60 12))

;; (define-key ctl-x-map "ti" 'timeclock-in)
;; (define-key ctl-x-map "to" 'timeclock-out)
;; (define-key ctl-x-map "tc" 'timeclock-change)
;; (define-key ctl-x-map "tg" 'timeclock-generate-report-by-day-by-project-new-buffer)
;; (define-key ctl-x-map "tr" 'timeclock-reread-log)
