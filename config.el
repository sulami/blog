;; -*- lexical-binding: t; -*-

(setq local-dir (or (getenv "CI_PWD")
                    (expand-file-name "~/src/sulami.github.io")))
(setq org-export-async-init-file (concat local-dir "/config.el"))
(setq target-dir (concat local-dir "/_site"))

(load-file (concat local-dir "/deps.el"))

(require 'dash)
(require 'f)
(require 's)
(require 'org)
(require 'ox)
(require 'ox-latex)
(require 'ox-rss)
(require 'ox-tufte)

(setq domain "sulami.xyz")
(setq url (concat "https://blog." domain))
(setq org-export-allow-bind-keywords t)
(setq org-export-async-debug nil)
(setq org-html-htmlize-output-type 'css)
(setq common-properties
      `(:author "Robin Schroer"
	    :email ,(concat "blog@" domain)
        :title "sulami's blog"
	    :section-numbers nil
	    :time-stamp-file nil
	    :with-drawers t
	    :with-toc nil
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

(defun blog/relative-link (file info)
  (file-relative-name (concat target-dir file) target-dir))

(defun modern-tufte-html-template (contents info)
  (concat
   "<!DOCTYPE html>\n"
   (format "<html lang=\"%s\">\n" (plist-get info :language))
   "<head>\n"
   (format "<meta charset=\"%s\">\n"
           (coding-system-get org-html-coding-system 'mime-charset))
   (format "<title>%s</title>\n"
           ;; NB String accidental HTML tags.
           (s-replace-regexp (rx "<" (+? anything) ">")
                             ""
                             (org-export-data (or (plist-get info :title) "") info)))
   (format "<meta name=\"author\" content=\"%s\">\n"
           (org-export-data (plist-get info :author) info))
   "<link rel=\"stylesheet\" href=\""
   ;; FIXME These don't work locally due to the prettified URLs(?)
   ;; (blog/relative-link "/css/tufte.css" info)
   "/css/tufte.css"
   "\" type=\"text/css\" />\n"
   "<link rel=\"stylesheet\" href=\""
   "/css/stylesheet.css"
   ;; (blog/relative-link "/css/stylesheet.css" info)
   "\" type=\"text/css\" />\n"
   "</head>\n"
   "<body>\n"
   "<input type=\"checkbox\" id=\"lightswitch\" />\n"
   "<div id=\"body\"><div id=\"container\">"
   "<div id=\"navigation\">"
   "<label for=\"lightswitch\" id=\"lightswitch-label\">☀</label>"
   " · <a href=\"" (blog/relative-link "/index.html" info) "\">Home</a>"
   " · <a href=\"" (blog/relative-link "/posts.html" info) "\">Archive</a>"
   " · <a href=\"../../atom.xml\">Feed</a>"
   " · <a href=\"" (blog/relative-link "/pages/about.html" info) "\">About</a>"
   " · <a href=\"" (blog/relative-link "/pages/cv.html" info) "\">CV</a> <a href=\"/pages/robin-schroer-cv.pdf\">(PDF)</a>"
   " · <a href=\"" (blog/relative-link "/pages/uses-this.html" info) "\">Uses This</a>"
   " · <a href=\"" (blog/relative-link "/pages/frankenwm.html" info) "\">FrankenWM</a>"
   "</div>"
   "<div id=\"content\"><article>"
   (format "<h1 class=\"title\">%s</h1>\n"
           (org-export-data (or (plist-get info :title) "") info))
   (when (plist-get info :date)
     (format "<div class=\"info\">Posted on %s</div>\n"
             (car (plist-get info :date))))
   contents
   "</article></div></div></div>
        <!-- Below is some JS, the only on this page. Things should be -->
        <!-- generally fine if you block it, but I'm adding notes so you -->
        <!-- can decide how you feel about each element. -->

        <!-- Privacy-aware analytics by https://www.goatcounter.com/ -->
        <!-- Stats are publicly available at https://sulami-blog.goatcounter.com/ -->
        <!-- <script data-goatcounter=\"https://sulami-blog.goatcounter.com/count\" async src=\"https://gc.zgo.at/count.js\"></script> -->

        <!-- Colour theme switcher -->
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
         :completion-function blog/prettify-urls
         :html-doctype "html5"
         :headline-levels 2
         :html-footnotes-section ""
         :html-html5-fancy t
         ,@common-properties)
        ("raw"
         :base-directory ,(concat local-dir "/raw")
         :publishing-directory ,(concat target-dir "/raw")
         :recursive t
         :publishing-function org-publish-attachment
         ,@common-properties)
        ("images"
         :base-directory ,(concat local-dir "/images")
         :publishing-directory ,(concat target-dir "/images")
         :recursive t
         :publishing-function org-publish-attachment
         ,@common-properties)
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
             (write-file (concat ,local-dir "/stylesheet.css"))))
         :completion-function
         (lambda (&rest args)
           "Cleanup the concatenated CSS file."
           (delete-file (concat ,local-dir "/stylesheet.css")))
         ,@common-properties)
        ("tufte"
         :base-directory ,(concat local-dir "/tufte")
         :recursive t
         :base-extension ,(rx (or "css"
                                  "eot"
                                  "svg"
                                  "ttf"
                                  "woff"))
         :publishing-directory ,(concat target-dir "/css")
         :publishing-function org-publish-attachment
         ,@common-properties)
        ("feed"
         :base-directory ,local-dir
         :html-footnotes-section ""
         :preparation-function
         (lambda (&rest args)
           "Combine all posts into a single file."
           (with-temp-buffer
             ;; gather all posts
             (cl-loop for post in (blog/all-posts)
                      do (let ((this-start (point-max)))
                           (goto-char (point-max))
                           ;; Insert the filename for later use as slug
                           (insert "\n:ATOM_PERMALINK: posts/"
                                   (f-base post)
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
         :html-link-home ,url
         ,@common-properties)
        ("cv"
         :base-directory ,local-dir
         :exclude ,(rx (1+ anything))
         :include (,(concat local-dir "/pages/cv.org"))
         :publishing-function org-latex-publish-to-pdf
         :completion-function
         (lambda (&rest args)
           (rename-file (concat target-dir "/pages/cv.pdf")
                        (concat target-dir "/pages/robin-schroer-cv.pdf")
                        t))
         :latex-class "scrartcl"
         :with-author nil
         :with-date nil
         ,@common-properties)
        ("website"
         :components ("posts-and-pages"
                      "images"
                      "raw"
                      "tufte"
                      "css"
                      "cv"
                      "feed"))))

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

(defun blog/all-posts ()
  "Returns all posts in chronological order (old -> new)"
  (-> (directory-files (concat local-dir "/posts")
                       t
                       (rx ".org" eos))
      (sort (lambda (x y)
              (string> (blog/file-date x)
                       (blog/file-date y))))))

(defun blog/global-word-count ()
  (-> (concat "wc -w "
              (apply #'concat (-interpose " " (blog/all-posts)))
              " | awk '/total/ {print $1}'")
      (shell-command-to-string)
      (s-trim)))

(defun blog/post-list (n)
  (->> (blog/all-posts)
       (-take n)
       (-map (lambda (p) (concat "- [["
                                 p
                                 "]["
                                 (blog/file-title p)
                                 "]] - "
                                 (blog/file-date p)
                                 "\n")))
       (apply #'concat)))

(defun blog/prettify-internal-links (from to)
  "Replace all HTML links to other pages with pretty links.

Writes to a new file."
  (with-temp-file to
    (insert-file from)
    ;; Rewrite index.html to /
    (goto-char 0)
    (while (re-search-forward (rx (zero-or-one "../")
                                  "index.html")
                              nil t)
      (replace-match "/" nil nil))
    ;; Rewrite all other link.html to link/
    (goto-char 0)
    (while (re-search-forward (rx "href=\""
                                  (group-n 1
                                           (or "../"     ;; Up to root pages
                                               ""        ;; Across pages in the same group
                                               "posts/"
                                               "pages/"))
                                  (group-n 2 (+? ascii))
                                  ".html\"")
                              nil t)
      (replace-match (cond
                      ;; Links down into categories
                      ((-contains-p (list "posts/" "pages/")
                                    (match-string 1))
                       "href=\"/\\1\\2/\"")
                      ;; Up to posts from nested pages
                      ((and (string= "" (match-string 1))
                            (string= "posts" (match-string 2)))
                       "href=\"/\\2\/\"")
                      ;; Links to the outside world
                      ((s-contains-p "/" (match-string 2))
                       "href=\"\\1\\2/\"")
                      ;; Links on the same level
                      ("href=\"../\\2/\""))
                     nil nil))))

(defun blog/prettify-urls (props)
  "/foo/index.html -> /foo/"
  ;; Index gets prettified in place.
  (let ((page (concat target-dir "/index.html")))
    (blog/prettify-internal-links page page))
  ;; Archive needs to be moved to posts/index.html.
  (let ((page (concat target-dir "/posts.html")))
    (blog/prettify-internal-links page (concat target-dir "/posts/index.html"))
    (f-delete page))
  ;; The rest:
  ;; Copy <base-name>.html to <base-name>/index.html and rewrite
  ;; internal links in posts and pages.
  (cl-loop for page in (-concat (directory-files (concat target-dir "/posts") t (rx ".html" eos))
                                (directory-files (concat target-dir "/pages") t (rx ".html" eos)))
           unless (string= page (concat target-dir "/posts/index.html"))
           do (let* ((new-dir-name (concat target-dir "/" (f-base (f-dirname page)) "/" (f-base page)))
                     (new-file-name (concat new-dir-name "/index.html")))
                (f-delete new-dir-name t)
                (make-directory new-dir-name)
                (blog/prettify-internal-links page new-file-name)
                (f-delete page))))
