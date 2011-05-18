(require 'el-expectations)


(expectations
  (expect t
    (let ((alist (qtref-build-classname-mapping-alist "./testdata.html")))
      (and 
       (string= "qabstractanimation.html" (cdr (assoc "QAbstractAnimation" alist)))
       (string= "qabstractbutton.html" (cdr (assoc "QAbstractButton" alist)))))))
