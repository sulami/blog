;; -*- lexical-binding: t; -*-

(setq debug-on-error t)
(setq local-dir (or (getenv "CI_PWD")
                    (expand-file-name "~/src/sulami.github.io")))
(defun ci-p ()
  (when (getenv "CI") t))
(setq org-export-async-init-file (concat local-dir "/config.el"))
(setq target-dir (concat local-dir "/_site"))

(load-file (concat local-dir "/deps.el"))

(require 'dash)
(require 'f)
(require 's)
(require 'org)
(require 'ox)
(require 'ox-latex)
(require 'ox-tufte)

(setq blog/domain "sulami.xyz")
(setq blog/url (concat "https://blog." blog/domain))
(setq blog/title "sulami's blog")
(setq org-export-allow-bind-keywords t)
(setq org-export-async-debug nil)
(setq org-html-htmlize-output-type 'css)
(setq blog/common-properties
      `(:author "Robin Schroer"
	    :email ,(concat "blog@" blog/domain)
        :title ,blog/title
	    :section-numbers nil
	    :time-stamp-file nil
	    :with-drawers t
	    :with-toc nil
        :wtih-tags t
        :html-doctype "html5"
        :html-head-include-default-style nil
        :html-head-include-scripts nil
        :html-html5-fancy t
        :html-metadata-timestamp-format "%e %B %Y"
        :publishing-directory ,target-dir))

(add-to-list 'org-latex-classes
             '("scrartcl" "\\documentclass[a4paper,10pt]{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(defun modern-tufte-html-template (contents info)
  (concat
   "<!DOCTYPE html>\n"
   (format "<html lang=\"%s\">\n" (plist-get info :language))
   "<head>\n"
   (format "<meta charset=\"%s\">\n"
           (coding-system-get org-html-coding-system 'mime-charset))
   "<meta name=\"viewport\" content=\"width=device-width\">\n"
   (format "<title>%s - %s</title>\n"
           ;; NB String accidental HTML tags.
           (s-replace-regexp (rx "<" (+? anything) ">")
                             ""
                             (org-export-data (or (plist-get info :title) "") info))
           blog/title)
   (format "<meta name=\"author\" content=\"%s\">\n"
           (org-export-data (plist-get info :author) info))
   (format "<link rel=alternate title=\"%s\" type=application/atom+xml href=\"/atom.xml\">\n"
           blog/title)
   "<link rel=\"stylesheet\" href=\"/css/stylesheet.css\" type=\"text/css\" />\n"
   "</head>\n"
   "<body>\n"
   "<input type=\"checkbox\" id=\"lightswitch\" />\n"
   "<div id=\"body\"><div id=\"container\">"
   "<div id=\"navigation\">"
   "<label for=\"lightswitch\" id=\"lightswitch-label\">☀</label>"
   " · <a href=\"/index.html\">Home</a>"
   " · <a href=\"/posts/index.html\">Archive</a>"
   " · <a href=\"../../atom.xml\">Feed</a>"
   " · <a href=\"/pages/about/index.html\">About</a>"
   " · <a href=\"/pages/cv/index.html\">CV</a> <a href=\"/pages/robin-schroer-cv.pdf\" data-goatcounter-click=\"cv-pdf\" data-goatcounter-title=\"CV PDF version\">(PDF)</a>"
   " · <a href=\"/pages/uses-this/index.html\">Uses This</a>"
   " · <a href=\"/pages/frankenwm/index.html\">FrankenWM</a>"
   "</div>"
   "<div id=\"content\"><article>"
   (format "<h1 class=\"title\">%s</h1>\n"
           (org-export-data (or (plist-get info :title) "") info))
   (when (plist-get info :date)
     (format "<div class=\"info\">Posted on %s</div>\n"
             (car (plist-get info :date))))
   (when (plist-get info :filetags)
     (format "<div class=\"info\">Tags: %s</div>\n"
             (->> (plist-get info :filetags)
                  (s-join " "))))
   contents
   "</article></div></div></div>
        <!-- Below is some JS, the only on this page. Things should be -->
        <!-- generally fine if you block it, but I'm adding notes so you -->
        <!-- can decide how you feel about each element. -->

        <!-- Privacy-aware analytics by https://www.goatcounter.com/ -->
        <!-- Stats are publicly available at https://sulami-blog.goatcounter.com/ -->"
   (when (ci-p)
     "<script data-goatcounter=\"https://sulami-blog.goatcounter.com/count\" async src=\"https://gc.zgo.at/count.js\"></script>")

   "<!-- Colour theme switcher -->
   <script>
     const themeSwitch = document.querySelector('#lightswitch');
     themeSwitch.checked = localStorage.getItem('switchedTheme') === 'true';

     themeSwitch.addEventListener('change', function (e) {
         if(e.currentTarget.checked === true) {
             // Add item to localstorage
             localStorage.setItem('switchedTheme', 'true');
         } else {
             // Remove item if theme is switched back to normal
             localStorage.removeItem('switchedTheme');
         }
     });
   </script>
   </body>\n"
   "</html>\n"))

(defun modern-tufte-html-section (section contents info)
  (let* ((headline (org-export-get-parent-headline section))
         (level (org-element-property :level headline)))
    (concat
     "<section>"
     (when headline
       (concat
        (format "<h%s>" (1+ level))
        ;; NB Fix for that one post that has subscript in headlines.
        (format "%s" (s-replace-regexp (rx "_{" (group-n 1 (1+ anything)) "}")
                                       "<sub>\\1</sub>"
                                       (org-element-property :raw-value headline)))
        (format "</h%s>\n" (1+ level))))
     contents
     "</section>\n")))

(defun modern-tufte-html-headline (headline contents info)
  contents)

(org-export-define-derived-backend 'modern-tufte-html 'tufte-html
  :translate-alist '((template . modern-tufte-html-template)
                     (section . modern-tufte-html-section)
                     (headline . modern-tufte-html-headline)
                     (item . org-html-item)))

(defun org-html-publish-to-modern-tufte-html (plist filename pub-dir)
  "Publish an org file to Tufte-styled HTML.
PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'modern-tufte-html filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension
                                      "html"))
                      plist pub-dir))

(setq org-publish-project-alist
      `(("posts-and-pages"
         :base-directory ,local-dir
         :recursive t
         :exclude "README.org"
         :publishing-function org-html-publish-to-modern-tufte-html
         :completion-function blog/prettify-internal-links
         :html-doctype "html5"
         :headline-levels 2
         :html-footnotes-section ""
         :html-html5-fancy t
         ,@blog/common-properties)
        ("raw"
         :base-directory ,(concat local-dir "/raw")
         :publishing-directory ,(concat target-dir "/raw")
         :recursive t
         :base-extension ,(rx (1+ anything))
         :publishing-function org-publish-attachment
         ,@blog/common-properties)
        ("images"
         :base-directory ,(concat local-dir "/images")
         :publishing-directory ,(concat target-dir "/images")
         :recursive t
         :base-extension ,(rx (1+ anything))
         :publishing-function org-publish-attachment
         ,@blog/common-properties)
        ("css"
         :base-directory ,local-dir
         :base-extension "css"
         :publishing-directory ,(concat target-dir "/css")
         :publishing-function org-publish-attachment
         :preparation-function
         (lambda (&rest args)
           "Concatenate all CSS files into a single one."
           (with-temp-buffer
             (cl-loop for file in (directory-files (concat ,local-dir "/css") t)
                      if (s-suffix-p ".css" file)
                      do (insert-file-contents file))
             (insert-file-contents (concat ,local-dir "/tufte/tufte.css"))
             (write-file (concat ,local-dir "/stylesheet.css"))))
         :completion-function
         (lambda (&rest args)
           "Cleanup the concatenated CSS file."
           (delete-file (concat ,local-dir "/stylesheet.css")))
         ,@blog/common-properties)
        ("tufte"
         :base-directory ,(concat local-dir "/tufte")
         :recursive t
         :base-extension ,(rx (or "eot"
                                  "svg"
                                  "ttf"
                                  "woff"))
         :publishing-directory ,(concat target-dir "/css")
         :publishing-function org-publish-attachment
         ,@blog/common-properties)
        ("favicon"
         :base-directory ,(concat local-dir "/favicons")
         :base-extension ,(rx (1+ anything))
         :publishing-directory ,target-dir
         :publishing-function org-publish-attachment
         ,@blog/common-properties)
        ("feed"
         :base-directory ,local-dir
         :html-footnotes-section ""
         :preparation-function
         (lambda (&rest args)
           "Combine all posts into a single file."
           (with-temp-buffer
             ;; gather all posts
             (cl-loop for post in (blog/all-posts 5)
                      do (let ((this-start (point-max)))
                           (goto-char (point-max))
                           ;; Insert the filename for later use as slug
                           (insert "\n:ATOM_PERMALINK: posts/"
                                   (f-base (f-parent post))
                                   "/\n")
                           (insert-file-contents post)
                           ;; Indent subheadings one extra level
                           (goto-char this-start)
                           (while (re-search-forward (rx line-start "*") nil t)
                             (replace-match "**" nil nil))))
             ;; Make titles new top level headings
             (goto-char 0)
             (while (re-search-forward (rx line-start "#+"
                                           (or "title" "TITLE")
                                           ":")
                                       nil t)
               (replace-match "*" nil nil))
             ;; Transform date format
             (goto-char 0)
             (while (re-search-forward (rx line-start "#+"
                                           (or "date" "DATE")
                                           ": "
                                           (group-n 1 (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
                                           line-end)
                                       nil t)
               (replace-match ":PROPERTIES:\n:PUBDATE: <\\1>\n:END:"
                              nil nil))
             ;; Swap the slug down into the properties section
             (goto-char 0)
             (while (re-search-forward (rx (group-n 1 (seq ":ATOM_PERMALINK: "
                                                           (+ not-newline)
                                                           "\n"))
                                           ;; Title section
                                           (group-n 2 (seq (+ not-newline)
                                                           "\n"
                                                           ":PROPERTIES:"
                                                           "\n")))
                                       nil t)
               (replace-match "\\2\\1" nil nil))
             ;; Write out the result
             (write-file (concat ,local-dir "/atom.org"))))
         :exclude ".*"
         :include ("atom.org")
         :publishing-function org-atom-publish-to-atom
         :html-link-use-abs-url t
         :html-link-home ,blog/url
         ,@blog/common-properties)
        ("cv"
         :base-directory ,local-dir
         :exclude ,(rx (1+ anything))
         :include (,(concat local-dir "/pages/cv/index.org"))
         :publishing-function org-latex-publish-to-pdf
         :completion-function
         (lambda (&rest args)
           (rename-file (concat target-dir "/pages/cv/index.pdf")
                        (concat target-dir "/pages/robin-schroer-cv.pdf")
                        t))
         :latex-class "scrartcl"
         :with-author nil
         :with-date nil
         ,@blog/common-properties)
        ("fast"
         :components ("posts-and-pages"
                      "images"
                      "raw"
                      "tufte"
                      "favicon"
                      "css"))
        ("all"
         :components ("posts-and-pages"
                      "images"
                      "raw"
                      "tufte"
                      "favicon"
                      "css"
                      "cv"
                      "feed"))))

;; Functions to call directly

(defun blog/render-all ()
  (org-publish "all" t))

(defun blog/render-fast ()
  (org-publish "fast" t))

(defun blog/render-file ()
  (org-publish-current-file)
  (blog/prettify-internal-links nil))

;; Macro Support

(defun blog/get-keyword-key-value (kwd)
  (let ((data (cadr kwd)))
    (list (plist-get data :key)
          (plist-get data :value))))

(defun blog/org-current-buffer-get-attr (attr)
  (nth 1 (assoc attr
                (org-element-map
                    (org-element-parse-buffer 'greater-element)
                    '(keyword)
                  #'blog/get-keyword-key-value))))

(defun blog/file-title (path)
  (with-current-buffer (find-file-noselect path)
    (blog/org-current-buffer-get-attr "TITLE")))

(defun blog/file-date (path)
  (with-current-buffer (find-file-noselect path)
    (blog/org-current-buffer-get-attr "DATE")))

(defun blog/file-tags (path)
  (with-current-buffer (find-file-noselect path)
    (-some->> (blog/org-current-buffer-get-attr "FILETAGS")
      (s-split " "))))

(defun blog/all-posts (n)
  "Returns all posts in chronological order (old -> new)"
  (-as-> (directory-files (concat local-dir "/posts")
                          t
                          nil
                          t)
         $
         (-filter #'f-directory-p $)
         (-filter (lambda (f) (not (s-contains-p "posts/." f))) $)
         (-map (lambda (f) (s-concat f "/index.org")) $)
         (-filter #'blog/file-date $)
         (sort $ (lambda (x y)
                   (string> (blog/file-date x)
                            (blog/file-date y))))
         (-take n $)))

(defun blog/global-word-count ()
  (-> (concat "wc -w "
              (apply #'concat (-interpose " " (blog/all-posts 1024)))
              " | awk '/total/ {print $1}'")
      (shell-command-to-string)
      (s-trim)))

(defun blog/post-list (n)
  (->> (blog/all-posts n)
       (-map (lambda (p) (concat "- [["
                                 p
                                 "]["
                                 (blog/file-title p)
                                 "]] - "
                                 (blog/file-date p)
                                 (if-let ((tags (blog/file-tags p)))
                                     (->> (s-join ", " tags)
                                          (format " (%s)"))
                                   "")
                                 "\n")))
       (apply #'concat)))

(defun blog/prettify-internal-links (_props)
  "Transform internal links from /foo/index.html to /foo/

Test cases:

href=\"/index.html\"
href=\"/foo/index.html\"
href=\"/foo/bar/index.html\"
href=\"foo/index.html\"
href=\"index.html\"

href=\"/\"
href=\"/foo/\"
href=\"/foo/bar/\"
href=\"/foo/\"
href=\"/\" "
  (cl-loop for page in (directory-files-recursively target-dir
                                                    (rx ".html" string-end))
           do (progn
                (find-file page)
                (while (re-search-forward (rx "href=\""
                                              (opt "/")
                                              (group-n 1
                                                       (0+ (any "a-zA-Z0-9-_/")))
                                              "index.html\"")
                                          nil t)
                  (replace-match "href=\"/\\1\""))
                (save-buffer))))
