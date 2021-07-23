(global-set-key (kbd "C-;") #'eval-last-sexp)

(global-set-key (kbd "s-x") #'clipboard-kill-region)
(global-set-key (kbd "s-c") #'clipboard-kill-ring-save)
(global-set-key (kbd "s-v") #'clipboard-yank)

(global-set-key
 (kbd "`")
 (lambda ()
   (interactive)
    (let ((last-command-event ?\())
      (call-interactively #'self-insert-command))))

(global-set-key (kbd "M-d") #'windmove-right)
(global-set-key (kbd "M-a") #'windmove-left)
(global-set-key (kbd "M-w") #'windmove-up)
(global-set-key (kbd "M-s") #'windmove-down)

(global-set-key (kbd "C-q") #'delete-other-windows)

(global-set-key (kbd "C-,") (lambda () (interactive) (scroll-down 3)))
(global-set-key (kbd "C-.") (lambda () (interactive) (scroll-up 3)))


(global-set-key (kbd "s-<f11>") #'xref-find-definitions)
(global-set-key (kbd "s-<f12>") #'xref-pop-marker-stack)

(global-set-key (kbd "s-=") #'company-complete)

(define-key emacs-lisp-mode-map (kbd "C-d") (lambda ()
                                              (interactive)
                                              (describe-function (function-called-at-point))))

(global-set-key (kbd "C-`") (lambda () (interactive) (insert "`")))
