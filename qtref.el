(defvar qtref-docroot "~/doc/qt/usr/share/qt4/doc/html/")
(defvar qtref-path-to-classes (concat qtref-docroot "classes.html"))
(defvar qtref-path-to-funcs (concat qtref-docroot "functions.html"))

(defvar qtref-classname-alist (qtref-build-classname-mapping-alist qtref-path-to-classes))
(defvar qtref-funcname-alist (qtref-build-funcname-mapping-alist qtref-path-to-funcs))

(defun qtref-build-classname-mapping-alist (path)
  "PATH should be some/path/to/classes.html"
  (when (file-exists-p (expand-file-name path))
    (let ((mapping-alist nil))
      (with-temp-buffer
        (insert-file-contents-literally (expand-file-name path))
        (goto-char (point-min))
        (while (re-search-forward "^<dd><a href=\"\\([^\">]+.html\\)\">\\([^<]+\\)</a></dd>$"
                                  nil
                                  t)
          (push (cons (match-string-no-properties 2) (match-string-no-properties 1))
                mapping-alist)))
      (nreverse mapping-alist))))

;; (car alist) => (funcname ((url1 . classname1) (url2 . classname2)))
(defun qtref-build-funcname-mapping-alist (path)
  (when (file-exists-p path)
    (let ((mapping-alist nil))
      (with-temp-buffer
        (insert-file-contents-literally (expand-file-name path))
        (goto-char (point-min))
        (unless (re-search-forward "functions.html" nil t)
          (error "qtref-build-funcname-mapping-alist failed"))
        (let ((fname nil))
          (while (re-search-forward "^<li>\\([^ :]+\\):\\([^\n]+\\)</li>$" nil t)
            (setq fname (match-string-no-properties 1)) ; function name
            (setq s (match-string-no-properties 2))
            (let ((start -1) (m nil))
              (while (setq start (string-match "<a href=\"\\([^\"]+\\)\">\\([^<]+\\)"
                                               s
                                               (incf start)))
                ;; url and classname
                (push (cons (match-string-no-properties 1 s) (match-string-no-properties 2 s))
                      m))
              (push (cons fname m) mapping-alist)))))
      (nreverse mapping-alist))))
