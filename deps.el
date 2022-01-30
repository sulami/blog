(defconst packages
  '(htmlize
    ;; org-plus-contrib
    ;; gnuplot
    ;; gnuplot-mode
    dash
    f
    s
    ox-tufte
    ox-rss
    ;; NB Some language modes are required for syntax highlighting.
    clojure-mode
    d-mode
    haskell-mode
    go-mode
    nix-mode))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (straight-pull-recipe-repositories)

(cl-loop for pkg in packages
         do (straight-use-package pkg))

(load-file "ox-atom.el")
