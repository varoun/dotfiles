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

;;; Manual Load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Prettify symbols. Displays lambda as the symbol for example.
(global-prettify-symbols-mode 1)

;;; Make M-/ Comments a region.

(global-set-key "/" 'comment-region)

;;; Org Mode
(load "~/org/orgsetup.el")

;;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


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

;; (add-to-list 'load-path "c:/Program Files/Erlang OTP/lib/tools-4.0/emacs")
;; (setq erlang-root-dir "C:/Program Files/Erlang OTP")
;; (add-to-list 'exec-path "C:/Program Files/Erlang OTP/bin")
;; (require 'erlang-start)


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
   '(geiser-chez geiser-mit geiser undo-tree slime paredit magit doom-themes company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "nil" :slant normal :weight semibold :height 135 :width normal)))))
