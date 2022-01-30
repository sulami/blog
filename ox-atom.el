;;; ox-atom.el --- Atom Back-End for Org Export Engine

;; Copyright (C) 2013-2021 Free Software Foundation, Inc.
;; Copyright (C) 2021  Robin Schroer

;; Author: Bastien Guerry <bzg@gnu.org>
;; Keywords: org, wp, blog, feed, atom

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Atom 2.0 back-end for Org exporter, based on
;; the `html' back-end.
;;
;; It requires Emacs 24.1 at least.
;;
;; It provides two commands for export, depending on the desired output:
;; `org-atom-export-as-atom' (temporary buffer) and `org-atom-export-to-atom'
;; (as a ".xml" file).
;;
;; This backend understands two new option keywords:
;;
;; #+Atom_EXTENSION: xml
;; #+Atom_IMAGE_URL: http://myblog.org/mypicture.jpg
;;
;; It uses #+HTML_LINK_HOME: to set the base url of the feed.
;;
;; Exporting an Org file to Atom modifies each top-level entry by adding a
;; PUBDATE property.  If `org-atom-use-entry-url-as-guid', it will also add
;; an ID property, later used as the guid for the feed's item.
;;
;; The top-level headline is used as the title of each Atom item unless
;; an Atom_TITLE property is set on the headline.
;;
;; You typically want to use it within a publishing project like this:
;;
;; (add-to-list
;;  'org-publish-project-alist
;;  '("homepage_atom"
;;    :base-directory "~/myhomepage/"
;;    :base-extension "org"
;;    :atom-image-url "http://lumiere.ens.fr/~guerry/images/faces/15.png"
;;    :html-link-home "http://lumiere.ens.fr/~guerry/"
;;    :html-link-use-abs-url t
;;    :atom-extension "xml"
;;    :publishing-directory "/home/guerry/public_html/"
;;    :publishing-function (org-atom-publish-to-atom)
;;    :section-numbers nil
;;    :exclude ".*"            ;; To exclude all files...
;;    :include ("index.org")   ;; ... except index.org.
;;    :table-of-contents nil))
;;
;; ... then rsync /home/guerry/public_html/ with your server.
;;
;; By default, the permalink for a blog entry points to the headline.
;; You can specify a different one by using the :ATOM_PERMALINK:
;; property within an entry.

;;; Code:

(require 'ox-html)
(declare-function url-encode-url "url-util" (url))

;;; Variables and options

(defgroup org-export-atom nil
  "Options specific to Atom export back-end."
  :tag "Org Atom"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-atom-image-url "http://orgmode.org/img/org-mode-unicorn-logo.png"
  "The URL of the an image for the Atom feed."
  :group 'org-export-atom
  :type 'string)

(defcustom org-atom-extension "xml"
  "File extension for the Atom 2.0 feed."
  :group 'org-export-atom
  :type 'string)

(defcustom org-atom-use-entry-url-as-guid t
  "Use the URL for the <guid> metatag?
When nil, Org will create ids using `org-icalendar-create-uid'."
  :group 'org-export-atom
  :type 'boolean)

;;; Define backend

(org-export-define-derived-backend 'atom 'html
  :menu-entry
  '(?r "Export to Atom"
       ((?R "As Atom buffer"
	    (lambda (a s v b) (org-atom-export-as-atom a s v)))
	(?r "As Atom file" (lambda (a s v b) (org-atom-export-to-atom a s v)))
	(?o "As Atom file and open"
	    (lambda (a s v b)
	      (if a (org-atom-export-to-atom t s v)
		(org-open-file (org-atom-export-to-atom nil s v)))))))
  :options-alist
  '((:description "DESCRIPTION" nil nil newline)
    (:keywords "KEYWORDS" nil nil space)
    (:with-toc nil nil nil) ;; Never include HTML's toc
    (:atom-extension "Atom_EXTENSION" nil org-atom-extension)
    (:atom-image-url "Atom_IMAGE_URL" nil org-atom-image-url))
  :filters-alist '((:filter-final-output . org-atom-final-function))
  :translate-alist '((headline . org-atom-headline)
		     (comment . (lambda (&rest args) ""))
		     (comment-block . (lambda (&rest args) ""))
		     (timestamp . (lambda (&rest args) ""))
		     (plain-text . org-atom-plain-text)
		     (section . org-atom-section)
		     (template . org-atom-template)))

;;; Export functions

;;;###autoload
(defun org-atom-export-as-atom (&optional async subtreep visible-only)
  "Export current buffer to a Atom buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Atom Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (org-icalendar-create-uid file 'warn-user)
    (org-atom-add-pubdate-property))
  (org-export-to-buffer 'atom "*Org Atom Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-atom-export-to-atom (&optional async subtreep visible-only)
  "Export current buffer to a Atom file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (org-icalendar-create-uid file 'warn-user)
    (org-atom-add-pubdate-property))
  (let ((outfile (org-export-output-file-name
		  (concat "." org-atom-extension) subtreep)))
    (org-export-to-file 'atom outfile async subtreep visible-only)))

;;;###autoload
(defun org-atom-publish-to-atom (plist filename pub-dir)
  "Publish an org file to Atom.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let ((bf (get-file-buffer filename)))
    (if bf
	  (with-current-buffer bf
	    (org-icalendar-create-uid filename 'warn-user)
	    (org-atom-add-pubdate-property)
	    (write-file filename))
      (find-file filename)
      (org-icalendar-create-uid filename 'warn-user)
      (org-atom-add-pubdate-property)
      (write-file filename) (kill-buffer)))
  (org-publish-org-to
   'atom filename (concat "." org-atom-extension) plist pub-dir))

;;; Main transcoding functions

(defun org-atom-headline (headline contents info)
  "Transcode HEADLINE element into Atom format.
CONTENTS is the headline contents.  INFO is a plist used as a
communication channel."
  (let ((level (org-export-get-relative-level headline info))
        (title (or (org-element-property :ATOM_TITLE headline)
		                  (replace-regexp-in-string
		                   org-bracket-link-regexp
		                   (lambda (m) (or (match-string 3 m)
				                           (match-string 1 m)))
		                   (org-element-property :raw-value headline)))))
    (unless (or (org-element-property :footnote-section-p headline))
      (if (> level 1)
          ;; Headlines below top-level become just headings.
          (format "<h%s>%s</h%s>\n%s"
                  level
                  title
                  level
                  contents)
        ;; Top-level headlines become feed items.
        (let* ((author (and (plist-get info :with-author)
			                (let ((auth (plist-get info :author)))
			                  (and auth (org-export-data auth info)))))
	           (htmlext (plist-get info :html-extension))
	           (hl-number (org-export-get-headline-number headline info))
	           (hl-home (file-name-as-directory (plist-get info :html-link-home)))
	           (hl-pdir (plist-get info :publishing-directory))
	           (hl-perm (org-element-property :ATOM_PERMALINK headline))
	           (anchor (org-export-get-reference headline info))
	           (category (org-atom-plain-text
		                  (or (org-element-property :CATEGORY headline) "") info))
	           (pubdate0 (org-element-property :PUBDATE headline))
	           (pubdate (let ((system-time-locale "C"))
		                  (if pubdate0
			                  (format-time-string "%FT%T%z" (org-time-string-to-time pubdate0)))))
	           (publink
	            (or (and hl-perm (concat (or hl-home hl-pdir) hl-perm))
		            (concat
		             (or hl-home hl-pdir)
		             (file-name-nondirectory
		              (file-name-sans-extension
		               (plist-get info :input-file))) "." htmlext "#" anchor)))
	           (guid (if org-atom-use-entry-url-as-guid
		                 publink
		               (org-atom-plain-text
		                (or (org-element-property :ID headline)
			                (org-element-property :CUSTOM_ID headline)
			                publink)
		                info))))
          (if (not pubdate0) "" ;; Skip entries with no PUBDATE prop
	        (format
             "<entry>
    <title>%s</title>
    <link href=\"%s\" />
    <id>%s</id>
    <published>%s</published>
    <updated>%s</updated>
    <summary type=\"html\"><![CDATA[%s]]></summary>
</entry>\n"
	         title publink guid pubdate pubdate contents)))))))

(defun org-atom-template (contents info)
  "Return complete document string after Atom conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (concat
   (format "<?xml version=\"1.0\" encoding=\"%s\"?>\n"
	   (symbol-name org-html-coding-system))
   "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n"
   (org-atom-build-channel-info info) "\n"
   contents
   "</feed>"))

(defun org-atom-build-channel-info (info)
  "Build the Atom channel information."
  (let* ((system-time-locale "C")
	 (title (plist-get info :title))
	 (email (org-export-data (plist-get info :email) info))
	 (author (and (plist-get info :with-author)
		      (let ((auth (plist-get info :author)))
			(and auth (org-export-data auth info)))))
	 (date (format-time-string "%FT%T%z"))
	 (description (org-export-data (plist-get info :description) info))
	 (lang (plist-get info :language))
	 (keywords (plist-get info :keywords))
	 (atomext (plist-get info :atom-extension))
	 (blogurl (or (plist-get info :html-link-home)
		      (plist-get info :publishing-directory)))
	 (image (url-encode-url (plist-get info :atom-image-url)))
	 (ifile (plist-get info :input-file))
	 (publink
	  (concat (file-name-as-directory blogurl)
		  (file-name-nondirectory
		   (file-name-sans-extension ifile))
		  "." atomext)))
    (format
     "<title>%s</title>
<link href=\"%s\" rel=\"self\" />
<link href=\"%s\" />
<id>%s</id>
<author>
    <name>%s</name>
    <email>%s</email>
</author>
<updated>%s</updated>\n"
     title publink blogurl publink author email date)))

(defun org-atom-section (section contents info)
  "Transcode SECTION element into Atom format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)

(defun org-atom-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to Atom.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-html-encode-plain-text
   (org-timestamp-translate timestamp)))

(defun org-atom-plain-text (contents info)
  "Convert plain text into Atom encoded text."
  (let (output)
    (setq output (org-html-encode-plain-text contents)
	  output (org-export-activate-smart-quotes
		  output :html info))))

;;; Filters

(defun org-atom-final-function (contents backend info)
  "Prettify the Atom output."
  (with-temp-buffer
    (xml-mode)
    (insert contents)
    (indent-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

;;; Miscellaneous

(defun org-atom-add-pubdate-property ()
  "Set the PUBDATE property for top-level headlines."
  (let (msg)
    (org-map-entries
     (lambda ()
       (let* ((entry (org-element-at-point))
	      (level (org-element-property :level entry)))
	 (when (= level 1)
	   (unless (org-entry-get (point) "PUBDATE")
	     (setq msg t)
	     (org-set-property
	      "PUBDATE" (format-time-string (cdr org-time-stamp-formats)))))))
     nil nil 'comment 'archive)
    (when msg
      (message "Property PUBDATE added to top-level entries in %s"
	       (buffer-file-name))
      (sit-for 2))))

(provide 'ox-atom)

;;; ox-atom.el ends here
