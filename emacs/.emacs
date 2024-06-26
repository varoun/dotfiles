;;;; Varoun's .emacs

;;;; All the NANO Emacs Stuff
(setq nano-font-family-monospaced "Roboto Mono")
(setq nano-font-family-proportional nil)
(setq nano-font-size 12)

;;; Copied from nano.el
;; Path to nano emacs modules (mandatory)
(add-to-list 'load-path "c:/Users/accou/apps/nano-emacs")
(add-to-list 'load-path ".")

;; Default layout (optional)
(require 'nano-layout)

;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
(add-to-list 'command-switch-alist '("-compact" . (lambda (args))))


;; Customize support for 'emacs -q' (Optional)
;; You can enable customizations by creating the nano-custom.el file
;; with e.g. `touch nano-custom.el` in the folder containing this file.
(let* ((this-file  (or load-file-name (buffer-file-name)))
       (this-dir  (file-name-directory this-file))
       (custom-path  (concat this-dir "nano-custom.el")))
  (when (and (eq nil user-init-file)
             (eq nil custom-file)
             (file-exists-p custom-path))
    (setq user-init-file this-file)
    (setq custom-file custom-path)
    (load custom-file)))

;; Theme
(require 'nano-faces)
(require 'nano-theme)
(require 'nano-theme-dark)
(require 'nano-theme-light)

(cond
 ((member "-default" command-line-args) t)
 ((member "-dark" command-line-args) (nano-theme-set-dark))
 (t (nano-theme-set-dark)))
(call-interactively 'nano-refresh-theme)

;; Nano default settings (optional)
(require 'nano-defaults)

;; Nano session saving (optional)
(require 'nano-session)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Nano key bindings modification (optional)
(require 'nano-bindings)

;; Compact layout (need to be loaded after nano-modeline)
(when (member "-compact" command-line-args)
  (require 'nano-compact))

;; Nano counsel configuration (optional)
;; Needs "counsel" package to be installed (M-x: package-install)
;; (require 'nano-counsel)

;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; Splash (optional)
(unless (member "-no-splash" command-line-args)
  (require 'nano-splash))

;; Help (optional)
(unless (member "-no-help" command-line-args)
  (require 'nano-help))

(provide 'nano)



;;;; Emacs config

;;; To fix signature errors
;; https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure

;;; Seq version errors
;; https://emacs.stackexchange.com/questions/80871/how-to-provide-updated-seq-package-to-magit

;;; UTF-8 related.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;; Prettify symbols. Displays lambda as the symbol for example.
(global-prettify-symbols-mode 1)


;;; Org Mode

;; Global behaviour
;;(setq org-startup-indented t)
(setq org-startup-folded t)

;; Org TODOs
(setq org-todo-keywords
      '((sequence "TODO(t)" "Scheduled(s)" "InProgress(i)" "Forwarded(f)"
		  "Delegated(d)" "|" "Killed(k)" "Completed(c)")))

;; Progress Log TODO state changes in a separate drawer.
(setq org-log-into-drawer t)
;; TODO sequences
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "c:/Users/accou/org/plan.org" "Tasks")
	 "** TODO %? %i [/]\n")))

;; Org Agenda
(setq org-agenda-files '("c:/Users/accou/org/plan.org"))
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Manual Load path
(add-to-list 'load-path "~/.emacs.d/lisp/")


;;; Zenburn Theme
;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

;;; Autocompletion using Company
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 1.0)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))

;;; Magit
(use-package magit
  :ensure t)

;;; Paredit
(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

;;; Undo
(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1))

;;; Cleanup whitespace
(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  ;(setq whitespace-line-column 80) ;; limit line length
  ;(setq whitespace-style '(face tabs empty trailing lines-tail))
  (setq whitespace-style '(empty trailing lines-tail)))

(use-package slime
  :ensure t
  :config
  ;(setq inferior-lisp-program "c:/Users/accou/apps/ccl-1.12.2-windowsx86/ccl/wx86cl64.exe")
  (setq inferior-lisp-program "sbcl"))


;;; Erlang

;; prevent command getting duplicated in output.
;; Some ref - https://github.com/emacs-ess/ess-stata-mode/issues/1
;;(setq comint-process-echoes t)

;; (add-to-list 'load-path "c:/Program Files/Erlang OTP/Lib/tools-4.0")
;; (setq erlang-root-dir "C:/Program Files/Erlang OTP")
;; (add-to-list 'exec-path "C:/Program Files/Erlang OTP/bin")
;; (require 'erlang-start)

(add-to-list 'load-path "c:/Program Files/Erlang OTP/lib/tools-4.0/emacs")
(setq erlang-root-dir "C:/Program Files/Erlang OTP")
(add-to-list 'exec-path "C:/Program Files/Erlang OTP/bin")
(require 'erlang-start)


;;; Scheme
(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(chez mit))
  (setq geiser-mit-binary "/usr/bin/mit-scheme")
  (setq geiser-chez-binary "/usr/bin/chezscheme"))

(use-package geiser-mit
  :ensure t)

(use-package geiser-chez
  :ensure t)
