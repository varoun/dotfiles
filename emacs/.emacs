;;;; Varoun's .emacs

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

;;; Make M-/ Comments a region.

(global-set-key "/" 'comment-region)

;;; Display line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; C++ Styles
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"   . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.c8$"   . c++-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.gen$" . gen-mode)
         ("\\.ms$" . fundamental-mode)
         ("\\.m$" . objc-mode)
         ("\\.mm$" . objc-mode))
	   auto-mode-alist))

;; C++ indentation style
(defconst varoun-c-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
    "Varoun's C++ Style")

;; CC++ mode handling
(defun varoun-c-hook ()
  ;; Set my style for the current buffer
  (c-add-style "varoun" varoun-c-style t)

  ;; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ;; Newline indents, semi-colon doesn't
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

  ;; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

  ;; Abbrevation expansion
  (abbrev-mode 1))

(add-hook 'c-mode-common-hook 'varoun-c-hook)
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
      '(("t" "Todo" entry (file+headline "~/org/plan.org" "Tasks")
	 "** TODO %? %i [/]\n")))

;; Org Agenda
(setq org-agenda-files '("~/org/plan.org"))
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

;;; Doom themes
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; (use-package clang-format
;;   :ensure t
;;   :config
;;   (setq clang-format-fallback-style "llvm"))


(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t))

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
  (setq inferior-lisp-program "sbcl"))


;;; Erlang

;; prevent command getting duplicated in output.
;; Some ref - https://github.com/emacs-ess/ess-stata-mode/issues/1
;;(setq comint-process-echoes t)

;; (setq comint-process-echoes t)
;; (add-to-list 'load-path "/usr/lib/erlang/lib/tools-3.5.3/emacs")
;; (setq erlang-root-dir "/usr/lib/erlang")
;; (add-to-list 'exec-path "/usr/lib/erlang/bin")
;; (require 'erlang-start)

(add-to-list 'load-path "c:/Program Files/Erlang OTP/lib/tools-4.1/emacs")
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(clang-format geiser-chez geiser-mit geiser slime undo-tree paredit magit company doom-themes))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code SemiBold" :foundry "outline" :slant normal :weight semi-bold :height 110 :width normal)))))
