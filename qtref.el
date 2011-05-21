;;; qtref.el --- An interface with Qt API reference on Emacs

;; Copyright (C) 2011  whitypig

;; Author: whitypig <whitypig@gmail.com>
;; Keywords: convenience, languages, help

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

;;; Commentary:

;; 

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'w3m)

;;;; Customization

(defcustom qtref-docroot nil
  "Path to the actual root of document directory, should not be a
  symbolic link. Should be end with a slash.")

;;;; Variables

(defvar qtref-path-to-classes nil
  "Absolute pathname to classes.html")

(defvar qtref-path-to-funcs nil
  "Absolute pathname to functions.html")

(defvar qtref-classname-alist nil
  "Association list that maps a classname to an url.")

(defvar qtref-funcname-alist nil
  "Association list that maps a function name to one or more urls.")

(defvar qtref-classnames nil
  "A list consisting of class names")

(defvar qtref-funcnames nil
  "A list consisting of function names")

;;;; Functions

(defun qtref-setup ()
  "Read document root directory from user, setting up paths, and
build alists."
  (interactive)
  ;; read docroot from user input
  (let* ((root (file-name-as-directory (read-directory-name "Path to document root: ")))
         (classes (concat root "classes.html"))
         (funcs (concat root "functions.html")))
    (unless (and (file-readable-p classes)
                 (file-readable-p funcs))
      (error "qtref: sorry, you probably entered the wrong docroot directory."))
    (setq qtref-docroot root
          qtref-path-to-classes classes
          qtref-path-to-funcs funcs)
    (setq qtref-classname-alist (qtref-build-classname-alist qtref-path-to-classes)
          qtref-funcname-alist (qtref-build-funcname-alist qtref-path-to-funcs))
    (setq qtref-classnames (loop for e in qtref-classname-alist
                                 collect (car e))
          qtref-funcnames (loop for e in qtref-funcname-alist
                                collect (car e)))))

(defun qtref-reset()
  (interactive)
  (setq qtref-docroot        nil  qtref-path-to-classes nil
        qtref-path-to-funcs  nil  qtref-classname-alist nil
        qtref-funcname-alist nil  qtref-classnames      nil
        qtref-funcnames      nil))

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

(defun qtref-visit-reference (path)
  (when path
    (let ((w (cond
              ((get-buffer-window "*w3m*")
               (get-buffer-window "*w3m*"))
              ((one-window-p)
               (split-window-vertically))
              (t
               (next-window)))))
      (select-window w)
      (w3m-find-file path))))

(defun qtref-classdoc ()
  (interactive)
  (unless qtref-docroot
    (qtref-setup))
  (let ((url (qtref-map-classname-to-url (qtref-read-classname))))
    (when url
      (qtref-visit-reference url))))

(defun qtref-funcdoc ()
  (interactive)
  (unless qtref-docroot
    (qtref-setup))
  (let* ((fname (qtref-read-funcname))
         (pairs (cdr (assoc fname qtref-funcname-alist)))
         (url (and pairs (qtref-get-func-url fname pairs))))
    (when url
      (qtref-visit-reference url))))

;; pairs => ((url1 . class1)) or ((url1 . class1) (url2 . class2))
(defun qtref-get-func-url (funcname pairs)
    (if (= (length pairs) 1)
        (concat qtref-docroot (caar pairs))
      (concat qtref-docroot
              (car (rassoc (qtref-select-func-classname
                            funcname
                            (loop for e in pairs
                                  collect (cdr e)))
                           pairs)))))

(defun qtref-select-func-classname (funcname names)
  (completing-read (concat funcname " in class: ")
                   names
                   nil
                   t
                   (qtref-leading-common-part-string names)))

(defun qtref-leading-common-part-string (lst)
  "Return the leading equal-part among strings in LST"
  (if (= (length lst) 1)
      (car lst)
    (reduce (lambda (s1 s2)
              (substring-no-properties s1
                                       0
                                       (1- (abs (compare-strings s1 0 nil s2 0 nil)))))
            lst)))

(provide 'qtref)
;;; qtref.el ends here
