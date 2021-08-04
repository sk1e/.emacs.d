;;; init.el --- emacs config initialization file   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  kotik

;; Author: kotik <kotik@kotik-one>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar straight-vc-git-default-protocol 'ssh)
(defvar straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(setq revert-without-query '(".*"))

(setq display-time-default-load-average nil)
(setq display-time-format "%H:%M, %a, %d")

(display-time-mode)

(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq-default indent-tabs-mode nil)

(add-hook 'minibuffer-setup-hook #'subword-mode)

(add-to-list 'load-path "~/.emacs.d/config")

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/Projects/"))
  (projectile-global-mode)
  (add-hook 'projectile-after-switch-project-hook
            #'treemacs-ext:init-bindings-from-storage)
  (add-hook 'projectile-after-switch-project-hook
            #'treemacs-ext:load-recent-files-storage))

(use-package company
  :config
  (add-hook 'prog-mode-hook #'company-mode))

(use-package magit)
(use-package s)
(use-package dash)

(load "global-bindings")
(load "utils")

(use-package file-template
  :straight (file-template :type git :host github :repo "sk1e/file-template")
  :config
  (mapc #'load (file-expand-wildcards "~/.emacs.d/config/file-templates/*.el")))

(load "treemacs-ext")

(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'before-save-hook #'whitespace-cleanup)

(use-package typescript-mode
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))
  (add-hook 'typescript-mode-hook #'subword-mode))

(use-package tree-sitter
  :hook ((typescript-mode . tree-sitter-hl-mode)
         (typescript-tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package lsp-mode
  :bind (("C-c f" . #'lsp-format-buffer)
         ("C-c r" . #'lsp-rename))
  :config
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'web-mode-hook #'lsp))

(use-package lsp-ui)

(use-package counsel
  :bind (("M-y" . #'counsel-yank-pop)
         ("C-x C-f" . #'counsel-find-file)
         ("C-h f" . #'counsel-describe-function)
         ("C-h v" . #'counsel-describe-variable)
         ("<menu>" . #'counsel-M-x)
         ("M-x" . #'counsel-M-x)
         ("C-s" . #'swiper)))

(load "ivy-rich-config")

(use-package helm
  :config
  (add-hook 'prog-mode 'helm-mode)
  :bind (("C-x b" . #'helm-buffers-list)
         ("C-c b" . #'helm-bookmarks)
         ("C-c i" . #'helm-imenu)))

(use-package helm-lsp
  :bind (("C-c d" . #'helm-lsp-diagnostics)
         ("s-a" . #'helm-lsp-code-actions)))

(use-package helm-projectile
  :bind (:map projectile-command-map
              ("p" . #'helm-projectile-switch-project)))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

(use-package rg
  :bind (("C-c s" . #'rg-menu)))

(use-package restart-emacs)
(use-package flycheck
  :bind (("<f9>" . #'flycheck-previous-error)
         ("<f10>" . #'flycheck-next-error)
         ("C-e" . #'flycheck-explain-error-at-point)))

(use-package which-key
  :config
  (add-hook 'text-mode-hook #'which-key-mode)
  (add-hook 'prog-mode-hook #'which-key-mode))

(use-package drag-stuff
  :bind (("M-<up>" . #'drag-stuff-up)
         ("M-<down>" . #'drag-stuff-down)
         ("M-<left>" . #'drag-stuff-left)
         ("M-<right>" . #'drag-stuff-right)))

(use-package yasnippet
  :config
  (yas-global-mode 1)

  (defun yas:ts:expand-named-import ()
    (interactive)
    (yas-expand-snippet (yas-lookup-snippet "ss-named-import")))

  (defun yas:ts:expand-aggregated-import ()
    (interactive)
    (yas-expand-snippet (yas-lookup-snippet "ss-aggregated-import")))

  (let ((import-keymap (make-sparse-keymap)))
    (define-key import-keymap (kbd "n") #'yas:ts:expand-named-import)
    (define-key import-keymap (kbd "a") #'yas:ts:expand-aggregated-import)

    (define-key typescript-mode-map (kbd "C-S-i") import-keymap)))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

(use-package undo-fu
  :bind (("C-z" . #'undo-fu-only-undo)
         ("C-S-z" . #'undo-fu-only-redo)))

(use-package hl-todo
  :config
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(js-indent-level 2)
 '(lsp-auto-guess-root t)
 '(lsp-headerline-breadcrumb-enable nil)
 '(make-backup-files nil)
 '(safe-local-variable-values '((git-commit-major-mode . git-commit-elisp-text-mode)))
 '(standard-indent 2)
 '(typescript-indent-level 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-comment-formats
   '(("java" . "/*")
     ("javascript" . "//")
     ("typescript" . "//")
     ("php" . "/*")
     ("css" . "/*")
     ("jsx" . "//")
     ("tsx" . "//")))
 '(web-mode-comment-style 2)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "gray15" :foreground "gainsboro" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(error ((t (:foreground "red3" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "CadetBlue4"))))
 '(font-lock-function-name-face ((t (:foreground "DodgerBlue1"))))
 '(font-lock-keyword-face ((t (:foreground "slate blue"))))
 '(font-lock-string-face ((t (:foreground "forest green"))))
 '(font-lock-type-face ((t (:foreground "DodgerBlue1"))))
 '(font-lock-variable-name-face ((t (:inherit default))))
 '(fringe ((t (:background "gray15"))))
 '(lsp-modeline-code-actions-face ((t (:foreground "dark cyan"))))
 '(lsp-treemacs-file-error ((t (:inherit nil :foreground "firebrick"))))
 '(minibuffer-prompt ((t (:foreground "SlateBlue1"))))
 '(mode-line ((t (:background "gray25" :foreground "white smoke" :box (:line-width -1 :style released-button)))))
 '(region ((t (:extend t :background "RoyalBlue4"))))
 '(typescript-primitive-face ((t (:inherit font-lock-type-face)))))
