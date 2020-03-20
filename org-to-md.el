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


(defun export-all ()
  (let ((content-dir "content")
        (ir-dir "_content"))
    (dolist (dirname (directory-files content-dir nil "[^.].+"))
      (make-directory (concat ir-dir "/" dirname) t))
    (let ((source-files (directory-files-recursively content-dir ".+\.org" nil))
          (hugo-base-dir ir-dir))
      (dolist (fp source-files)
        (message (concat "reading " fp))
        (with-temp-buffer
          (insert-file-contents fp)
          (let ((output (org-export-as 'hugo nil nil nil '(:hugo-base-dir "_content" :hugo-front-matter-format "yaml")))
                (outfile (concat ir-dir (seq-drop fp (length (expand-file-name content-dir))))))
            (message (concat "writing " outfile))
            (append-to-file output nil outfile)))))))
