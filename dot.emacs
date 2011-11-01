; -*- mode: lisp; -*-

(setq load-path
      (append (list nil 
		    "~ivan/dotfiles/")
	      load-path))

(require 'git)
(require 'xcscope)

(autoload 'doc-mode "doc-mode")
(setq js2-mirror-mode nil)
(autoload 'javascript-mode "javascript" nil t)
(autoload 'graphviz-dot-mode "graphviz-dot-mode")
(load "nxml-mode-20041004/rng-auto.el")


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
		("\\.erb$"    . html-mode)
		("\\.hi$"     . haskell-mode)
		("\\.hsc$"    . haskell-mode)
		("\\.js$"     . javascript-mode)
		("\\.l[hg]s$" . literate-haskell-mode)
		("\\.proto$"  . protobuf-mode)
		("\\.rhtml$"  . html-mode)
		("\\.xml$"    . nxml-mode)
		("\\.xsl$"    . nxml-mode)
		)))

(setq completion-ignored-extensions
      (append completion-ignored-extensions
	      '(
		".d"
		".x"
		)))

;; turn on code folding
(defun enable-folding ()
  (hs-minor-mode t)
  (local-set-key (kbd "C-c =") 'hs-show-block)
  (local-set-key (kbd "C-c +") 'hs-show-all)
  (local-set-key (kbd "C-c -") 'hs-hide-block)
  (local-set-key (kbd "C-c _") 'hs-hide-all))

;; c-mode
(defun my-c-mode-common-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq indent-tabs-mode nil)
  (enable-folding)
  ;; originally copied from http://www.patd.net/~ciggieposeur/other/startup.el
  (setq c-offsets-alist '((innamespace . 0)		;; Don't indent for namespace
			  (defun-block-intro . +)	;; Do indent for functions
			  (statement-block-intro . +)	;; Do indent for statementblocks
			  (statement . 0)		;; Don't indent on individual statements
			  (statement-cont . +)		;; Don't indent on individual statements
			  (substatement-open . 0)	;; Do indent for conditional blocks
			  (substatement . +)		;; Do indent for 1-line if blocks
			  (else-clause . 0)		;; Don't indent for the else clause
			  (block-close . 0)		;; Don't indent for the block closing }
			  (cpp-macro . [0])		;; Don't indent inside #ifdefs
			  (cpp-macro-cont . 0)		;; Don't indent inside #ifdefs
			  (inclass . +)			;; Do indent inside class declarations
			  (case-label . 0)		;; Do indent for case labels
			  (access-label . -)		;; Don't indent public/private
			  (statement-case-intro . +)	;; Do indent for case blocks
			  (statement-case-open . +)	;; Do indent for case open
			  (comment-intro . 0)		;; Don't indent for comments
			  (c . 1)			;; Don't indent for comment continuations
			  (brace-list-intro . +)	;; Do indent the first line in enums
			  (brace-list-entry . 0)	;; Do not indent additional lines inside enums
			  (brace-entry-open . 0)	;; Do not indent additional lines inside brace lists
			  (brace-list-close . 0)	;; Go back after brace list closes
			  (topmost-intro-cont . 0)	;; top-level continued (like argument in function pointer typedef)
			  (arglist-intro . +)		;; Do indent initial argument
			  (arglist-cont-nonempty . +)	;; Do indent additional argument lines
			  (arglist-close . 0)		;; Do not indent closing paren
			  )))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;; function keys
(global-set-key [f1] 'cscope-find-global-definition)
(global-set-key [f2] 'cscope-find-this-text-string)
(global-set-key [f3] 'manual-entry)
(global-set-key [f4] 'dabbrev-expand)
(global-set-key [f5] 'kill-this-buffer)
(global-set-key [f6] 'buffer-menu)
(global-set-key [f7] 'font-lock-fontify-buffer)
(global-set-key [f8] 'indent-region)
(global-set-key [f9] 'next-error)
(global-set-key [f10] 'compile)
(global-set-key "g" 'goto-line)

