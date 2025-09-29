;;; treemacs-ext.el --- treemacs initialization and extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  kotik

;; Author: kotik <kotik@kotik-one>
;; Keywords: tools

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

(defun treemacs-ext:show-tree ()
  (interactive)
  (let ((win (selected-window)))
    (treemacs)
    (select-window win)))

(defun treemacs-ext:kill-buffers-for-deleted-node (path)
  (let ((files-with-buffers-to-kill
         (-filter (lambda (x) (s-starts-with? path x))
                  (mapcar #'buffer-file-name (buffer-list)))))
    (mapc (lambda (x) (kill-buffer (get-file-buffer x)))
          files-with-buffers-to-kill)))

(use-package treemacs
  :bind (("M-t" . treemacs-ext:show-tree))
  :requires (projectile file-template)
  :config
  (add-hook 'treemacs-delete-file-functions #'treemacs-ext:kill-buffers-for-deleted-node)

  (add-to-list 'treemacs-litter-directories "/straight")

  (let ((treemacs-outside-map (make-sparse-keymap)))
    (define-key treemacs-outside-map "l" #'treemacs-cleanup-litter)
    (define-key treemacs-outside-map "e" #'treemacs-edit-workspaces)
    (define-key treemacs-outside-map "s" #'treemacs-switch-workspace)
    (define-key prog-mode-map (kbd "C-c t") treemacs-outside-map)
    (define-key text-mode-map (kbd "C-c t") treemacs-outside-map))

  (let ((space-map (make-sparse-keymap)))
    (mapc (lambda (key) (define-key space-map (kbd key) (treemacs-ext:make-node-binder key)))
          '("<f1>" "<f2>" "<f3>" "1" "2" "3" "4"))
    (define-key treemacs-mode-map (kbd "SPC") space-map))
  (define-key treemacs-mode-map (kbd "`") #'treemacs-goto-parent-node)
  (define-key treemacs-mode-map (kbd "[") (treemacs-ext:make-local-neighbour-opener-from-navigator #'treemacs-previous-neighbour 1))
  (define-key treemacs-mode-map (kbd "]") (treemacs-ext:make-local-neighbour-opener-from-navigator #'treemacs-next-neighbour 1))
  ;; (define-key treemacs-mode-map (kbd "M-[") (treemacs-ext:make-local-neighbour-opener-from-navigator #'treemacs-previous-neighbour 4))
  ;; (define-key treemacs-mode-map (kbd "M-]") (treemacs-ext:make-local-neighbour-opener-from-navigator #'treemacs-next-neighbour 4))
  (define-key treemacs-mode-map (kbd "tc") (ft:make-template-expander ft:templates:component))
  (define-key treemacs-mode-map (kbd "tf") (ft:make-template-expander ft:templates:simple-feature))
  (define-key treemacs-mode-map (kbd "tp") (ft:make-template-expander ft:templates:page))
  (define-key treemacs-mode-map (kbd "ti") (ft:make-template-expander ft:templates:i18n-shared-references))
  ;; (global-set-key (kbd "s-`") (lambda ()
  ;;                               (interactive)
  ;;                               (with-selected-window (treemacs-get-local-window)
  ;;                                 (treemacs-goto-parent-node)
  ;;                                 (treemacs-goto-node (treemacs-safe-button-get (treemacs-current-button) :path)))))
  ;; (global-set-key (kbd "M-[") (treemacs-ext:make-neighbour-opener-from-navigator #'treemacs-previous-neighbour 1))
  ;; (global-set-key (kbd "M-]") (treemacs-ext:make-neighbour-opener-from-navigator #'treemacs-next-neighbour 1))
  (global-set-key (kbd "M-{") (treemacs-ext:make-neighbour-opener-from-navigator #'treemacs-previous-neighbour 4))
  (global-set-key (kbd "M-}") (treemacs-ext:make-neighbour-opener-from-navigator #'treemacs-next-neighbour 4))
  (global-set-key (kbd "M-w") (treemacs-ext:make-line-opener-from-navigator #'treemacs-previous-line))
  (global-set-key (kbd "M-s") (treemacs-ext:make-line-opener-from-navigator #'treemacs-next-line))
  (global-set-key (kbd "M-SPC") (lambda ()
                                  (interactive)
                                  (with-selected-window (treemacs-get-local-window)
                                    (treemacs-RET-action)))))

(defun treemacs-ext:make-line-opener-from-navigator (navigator)
  (lambda ()
    (interactive)
    (with-selected-window (treemacs-get-local-window)
      (funcall navigator 1)
      (treemacs-goto-node (treemacs-safe-button-get (treemacs-current-button) :path)))))

(defun treemacs-ext:make-neighbour-opener-from-navigator (navigator count)
  (lambda ()
    (interactive)
    (with-selected-window (treemacs-get-local-window)
      (cl-loop repeat count
               do (funcall navigator))
      (treemacs-goto-node (treemacs-safe-button-get (treemacs-current-button) :path)))))

(defun treemacs-ext:make-local-neighbour-opener-from-navigator (navigator count)
  (lambda ()
    (interactive)
    (cl-loop repeat count
             do (funcall navigator))))

(defun treemacs-ext:make-node-opener (path)
  (lambda ()
    (interactive)
    (cond
     ((file-directory-p path) (treemacs-ext:goto-node (or (treemacs-ext:get-recent-descendant path) path)))
     (t (treemacs-ext:goto-node path)))))

(defun treemacs-ext:goto-node (path)
  (with-selected-window (treemacs-get-local-window)
    (treemacs-goto-node path)
    (treemacs-visit-node-default)))

(defvar treemacs-ext:recent-project-files nil)

(defun treemacs-ext:save-current-buffer-file-path (&optional _)
  (when-let ((path (buffer-file-name (current-buffer))))
    (setq treemacs-ext:recent-project-files (cons path (cl-delete path treemacs-ext:recent-project-files)))
    (treemacs-ext:save-recent-files-storage treemacs-ext:recent-project-files)))

(advice-add 'treemacs-visit-node-default :after #'treemacs-ext:save-current-buffer-file-path)

(defun treemacs-ext:get-recent-descendant (path)
  (cl-find-if (lambda (x) (s-starts-with-p path x))
              treemacs-ext:recent-project-files))

(defun treemacs-ext:make-node-binder (key)
  (lambda ()
    (interactive)
    (let ((storage (treemacs-ext:load-binding-storage))
          (path (treemacs-safe-button-get (treemacs-current-button) :path)))
      (treemacs-ext:save-binding-storage (cons (cons key path)
                                               (assoc-delete-all key storage)))
      (global-set-key (kbd (format "M-%s" key)) (treemacs-ext:make-node-opener path)))))

(defconst treemacs-ext:binding-storage-filename ".binding-storage")
(defconst treemacs-ext:recent-files-storage-filename ".recent-files-storage")

(defun treemacs-ext:init-bindings-from-storage ()
  (mapc (lambda (binding-pair)
          (cl-destructuring-bind (key . path) binding-pair
            (global-set-key (kbd (format "M-%s" key)) (treemacs-ext:make-node-opener path))))
        (treemacs-ext:load-binding-storage)))

(defun treemacs-ext:get-binding-storage-path ()
  (projectile-expand-root treemacs-ext:binding-storage-filename))

(defun treemacs-ext:get-recent-files-storage-path ()
  (projectile-expand-root treemacs-ext:recent-files-storage-filename))

(defun treemacs-ext:save-binding-storage (storage-data)
  (with-temp-file (treemacs-ext:get-binding-storage-path)
    (insert (format "%S" storage-data))))

(defun treemacs-ext:save-recent-files-storage (storage-data)
  (with-temp-file (treemacs-ext:get-recent-files-storage-path)
    (insert (format "%S" storage-data))))

(defun treemacs-ext:load-binding-storage ()
  (cond
   ((file-exists-p (treemacs-ext:get-binding-storage-path))
    (with-temp-buffer
      (insert-file-contents (treemacs-ext:get-binding-storage-path))
      (read (current-buffer))))
   (t '())))

(defun treemacs-ext:load-recent-files-storage ()
  (setq treemacs-ext:recent-project-files
        (cond
         ((file-exists-p (treemacs-ext:get-recent-files-storage-path))
          (with-temp-buffer
            (insert-file-contents (treemacs-ext:get-recent-files-storage-path))
            (read (current-buffer))))
         (t '()))))

(provide 'treemacs-ext)
;;; treemacs-ext.el ends here
