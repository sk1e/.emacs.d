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

(setq auto-save-default nil)
(setq create-lockfiles nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(setq-default indent-tabs-mode nil)

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

(load "global-bindings")
(load "treemacs-ext")

(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'before-save-hook #'whitespace-cleanup)

(use-package typescript-mode)
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.[jt]sx\\'" . web-mode)))

(use-package lsp-mode
  :bind (("C-c f" . #'lsp-format-buffer))
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
  :bind (("C-c d" . #'helm-lsp-diagnostics)))

(use-package helm-projectile)
(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

(use-package rg
  :bind (("C-c s" . #'rg-menu)))

(use-package restart-emacs)
(use-package flycheck
  :bind (("<f9>" . #'flycheck-previous-error)
         ("<f10>" . #'flycheck-next-error)))

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

  (add-hook 'web-mode-hook (lambda () (add-to-list 'company-backends '(company-capf company-yasnippet)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-auto-guess-root t)
 '(lsp-headerline-breadcrumb-enable nil)
 '(make-backup-files nil)
 '(safe-local-variable-values '((git-commit-major-mode . git-commit-elisp-text-mode)))
 '(standard-indent 2)
 '(web-mode-code-indent-offset 2)
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
 '(font-lock-variable-name-face ((t (:inherit default))))
 '(fringe ((t (:background "gray15"))))
 '(lsp-modeline-code-actions-face ((t (:foreground "dark cyan"))))
 '(lsp-treemacs-file-error ((t (:inherit nil :foreground "firebrick"))))
 '(minibuffer-prompt ((t (:foreground "SlateBlue1")))))
