(eval-when-compile
  (require 'cl))
(require 'w3m)

(defcustom qtref-docroot "/usr/share/qt4/doc/html/"
  "Path to the actual root of document directory, should not be a
  symbolic link. Should be end with a slash.")

(defvar qtref-path-to-classes (concat qtref-docroot "classes.html"))
(defvar qtref-path-to-funcs (concat qtref-docroot "functions.html"))

(defvar qtref-classname-alist (qtref-build-classname-alist qtref-path-to-classes)
  "Association list that maps a classname to an url.")

(defvar qtref-funcname-alist (qtref-build-funcname-alist qtref-path-to-funcs)
  "Association list that maps a function name to one or more urls.")

(defvar qtref-classnames (loop for e in qtref-classname-alist
                               collect (car e)))

(defvar qtref-funcnames (loop for e in qtref-funcname-alist
                              collect (car e)))

;; (car alist) => (classname . url)
(defun qtref-build-classname-alist (path)
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

(defun qtref-set-classname-alist (path)
  (interactive "fPath to classes.html: ")
  (and (string= "classes.html" (ff-basename path))
       (setq qtref-classname-alist (qtref-build-classname-alist path))))

;; (car alist) => (funcname ((url1 . classname1) (url2 . classname2)))
(defun qtref-build-funcname-alist (path)
  (when (file-exists-p path)
    (let ((mapping-alist nil))
      (with-temp-buffer
        (insert-file-contents-literally (expand-file-name path))
        (goto-char (point-min))
        (unless (re-search-forward "functions.html" nil t)
          (error "qtref: qtref-build-funcname-mapping-alist failed"))
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
              (push (cons fname (nreverse m)) mapping-alist)))))
      (nreverse mapping-alist))))

(defun qtref-set-funcnames-alist (path)
  (interactive "fPath to functions.html: ")
  (and (string= "functions.html" (ff-basename path))
       (setq qtref-build-funcname-alist path)))

(defun qtref-read-classname ()
  (completing-read "Class: " qtref-classnames nil t (thing-at-point 'symbol)))

(defun qtref-read-funcname ()
  (completing-read "Function: " qtref-funcnames nil t (thing-at-point 'symbol)))

(defun qtref-map-classname-to-url (classname)
  (let ((url (cdr (assoc classname qtref-classname-alist))))
    (if url
        (concat qtref-docroot url)
      (message "qtref: cannot find a html with a name of %s " classname)
      nil)))

(defun qtref-classdoc ()
  (interactive)
  (let ((url (qtref-map-classname-to-url (qtref-read-classname))))
    (when url
      (w3m-find-file url))))

;; TODO implement
(defun qtref-funcdoc ()
  (interactive)
  (error "Not implemented yet"))
