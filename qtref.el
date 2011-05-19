;;; qtref.el --- An interface with Qt API reference on Emacs

;; Copyright 2011 whitypig <whitypig@gmail.com>
;;

;; Author: whtiypig <whitypig@gmail.com>

;; Keywords: C++, Qt
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;;; Code

(eval-when-compile
  (require 'cl))
(require 'w3m)

;;;; Customization

(defcustom qtref-docroot "/usr/share/qt4/doc/html/"
  "Path to the actual root of document directory, should not be a
  symbolic link. Should be end with a slash.")

;;;; Variables

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

;;;; Functions

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
  (interactive)
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

(defun qtref-funcdoc ()
  (interactive)
  (let* ((pairs (cdr (assoc (qtref-read-funcname) qtref-funcname-alist)))
         (url (and pairs (qtref-get-func-url pairs))))
    (when url
      (w3m-find-file url))))

;; pairs => ((url1 . class1)) or ((url1 . class1) (url2 . class2))
(defun qtref-get-func-url (pairs)
  (let ((len (length pairs)))
    (if (= len 1)
        (concat qtref-docroot (caar pairs))
      (concat qtref-docroot
              (car (rassoc (qtref-select-func-classname (loop for e in pairs
                                                              collect (cdr e)))
                           pairs))))))

(defun qtref-select-func-classname (names)
  (completing-read "in class: " names nil t))

(provide 'qtref)
;;; qtref.el ends here
