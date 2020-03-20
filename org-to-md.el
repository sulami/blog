(defun org-to-md ()
  (let ((org-document-content "")
        this-read)
    (while (setq this-read (ignore-errors
                             (read-from-minibuffer "")))
      (setq org-document-content (concat org-document-content "\n" this-read)))
    (princ (org-export-string-as org-document-content
                                 'hugo
                                 nil
                                 '(:hugo-front-matter-format "yaml")))))
