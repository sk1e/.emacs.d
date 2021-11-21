(defun buffer-file-name/no-extension ()
  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))

(defun scss:add-style-use-if-none ()
  (save-excursion
    (unless (re-search-backward (regexp-quote "@use \"style/colors\";") nil t)
      (cond
       ((re-search-backward (regexp-quote "@use") nil t)
        (message ">> found use")
        (end-of-line) (insert "\n@use \"style/colors\";\n" ))
       (t
        (goto-char 1)
        (message ">> insert new")
        (insert "@use \"style/colors\";\n\n" ))))))

(defun camel-case->lisp-case (str)
  (let ((case-fold-search nil))
    (mapconcat (lambda (x) (downcase (car x)))
               (s-match-strings-all (rx (: upper-case (+ lower-case)))
                                    str)
               "-")))

(defun containing-directory ()
  (file-name-base
   (directory-file-name
    (file-name-directory   (buffer-file-name (current-buffer))))))

(defun px-to-rem (arg)
  (interactive "n? ")
  (insert (format "%.2frem" (/ arg 14.0))))

(defun all-px->rem ()
  (interactive)
  (save-excursion
    (goto-char 1)
    (while (re-search-forward (rx (1+ digit) (? "." (0+ digit)) "px") nil t)
      (cl-destructuring-bind (start-point end-point) (mapcar #'marker-position (match-data))
        (let ((number (string-to-number (buffer-substring start-point (- end-point 2)))))
          (when (> number 4)
            (replace-match (format "%.2frem" (/ number 14.0)))))))))

(provide 'utils)
