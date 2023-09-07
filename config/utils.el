(defun buffer-file-name/no-extension ()
  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))

(defun scss:add-style-use-if-none ()
  (save-excursion
    (unless (re-search-backward (regexp-quote "@use 'style/colors';") nil t)
      (cond
       ((re-search-backward (regexp-quote "@use") nil t)
        (end-of-line) (insert "\n@use 'style/colors';\n" ))
       (t
        (goto-char 1)
        (insert "@use \'style/colors\';\n\n" ))))))

(defun scss:add-device-use-if-none ()
  (save-excursion
    (unless (re-search-backward (regexp-quote "@use 'style/device';") nil t)
      (cond
       ((re-search-backward (regexp-quote "@use") nil t)
        (end-of-line) (insert "\n@use 'style/device';\n" ))
       (t
        (goto-char 1)
        (insert "@use \'style/device\';\n\n" ))))))


(defun scss:add-sizes-use-if-none ()
  (save-excursion
    (unless (re-search-backward (regexp-quote "@use 'style/sizes';") nil t)
      (cond
       ((re-search-backward (regexp-quote "@use") nil t)
        (end-of-line) (insert "\n@use 'style/sizes';\n" ))
       (t
        (goto-char 1)
        (insert "@use \'style/sizes\';\n\n" ))))))


(defun camel-case->lisp-case (str)
  (let ((case-fold-search nil))
    (mapconcat (lambda (x) (downcase (car x)))
               (s-match-strings-all (rx (: upper-case (+ lower-case)))
                                    str)
               "-")))

(defun containing-directory ()
  (file-name-base
   (directory-file-name
    (file-name-directory (buffer-file-name (current-buffer))))))

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




(defun get-feature-absolute-path ()
  (interactive)
  (let ((case-fold-search nil))
    (cl-second (s-match (rx (group "features" (+? anychar) "/" upper-case (+? anychar)) "/")
                        (buffer-file-name (current-buffer))))))

;; (file-name-base (buffer-file-name (current-buffer)))

(defun get-feature-relative-path ()
  (interactive)
  (let* ((case-fold-search nil)
         (path (buffer-file-name (current-buffer)))
         (file-name (concat (file-name-base path)
                            "."
                            (file-name-extension path)))
         (match (cl-second (s-match
                            (rx "features" (+? anychar) "/" upper-case (+? anychar) "/" (group (+? anychar)) (literal file-name))
                            path)))
         (depth (cond
                 ((eq match nil) 0)
                 (t (s-count-matches (rx "/") match)))))

    (cond
     ((eq depth 0) "./")
     (t (s-repeat depth "../")))
    ))

;; (s-match (rx "hello " (group "world")) "hello world")
(defun ts:insert-i18n-data-absolute-import ()
  (interactive)
  (let ((text (concat "import i18nData from '" (get-feature-absolute-path) "/i18n.json';\n")))
    (save-excursion
      (unless (re-search-backward (rx "import i18nData") nil t)
        (goto-char 0)
        (insert text)))))

(defun ts:insert-i18n-data-relative-import ()
  (interactive)
  (let ((text (concat "import i18nData from '" (get-feature-relative-path) "i18n.json';\n")))
    (save-excursion
      (unless (re-search-backward (rx "import i18nData") nil t)
        (goto-char 0)
        (insert text)))))

(provide 'utils)
