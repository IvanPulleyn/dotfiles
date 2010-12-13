; -*- mode: lisp; -*-

(setq load-path (cons "~ivan/dotfiles" load-path))

(require 'git)
(require 'xcscope)

(autoload 'doc-mode "doc-mode")
(autoload 'javascript-mode "javascript" nil t)
(autoload 'graphviz-dot-mode "graphviz-dot-mode")

(global-font-lock-mode 1)
(setq indent-tabs-mode nil)
(setq c-default-style "bsd")
(setq c-basic-offset 4)

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
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; function keys
(global-set-key [f1] 'cscope-find-global-definition)
(global-set-key [f2] 'cscope-find-this-symbol)
(global-set-key [f3] 'manual-entry)
(global-set-key [f4] 'dabbrev-expand)
(global-set-key [f5] 'kill-this-buffer)
(global-set-key [f6] 'buffer-menu)
(global-set-key [f7] 'font-lock-fontify-buffer)
(global-set-key [f8] 'indent-region)
(global-set-key [f9] 'next-error)
(global-set-key [f10] 'compile)

