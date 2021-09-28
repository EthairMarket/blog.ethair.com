;; ;; publish.el --- Org blog -*- lexical-binding: t -*-

;; Author: Boogs Idris
;; URL: https://github.com/EthairMarket/blog.ethair.com

;;; Commentary:
;; This script will convert the org-mode files in the source directory
;; into the appropriate html files.

;;; Code:

(require 'ox-publish)
(require 'seq)

;; RSS and atom webfeed generator for Emacs
(add-to-list 'load-path "emacs-webfeeder")
(if (require 'webfeeder nil 'noerror)
    (call-process "git" nil nil nil "-C" "emacs-webfeeder" "pull")
  (call-process "git" nil nil nil "clone" "https://gitlab.com/ambrevar/emacs-webfeeder")
  (require 'webfeeder))

(defvar ethair/repository "https://github.com/EthairMarket/blog.ethair.com")
(defvar ethair/root (expand-file-name "."))

(setq org-publish-use-timestamps-flag t
      org-publish-timestamp-directory "./")

(setq make-backup-files nil)

(setq org-export-with-section-numbers nil
      org-export-with-smart-quotes t
      org-export-with-email t
      org-export-with-date t)

(defun ethair/git-first-date (file)
  "Return the first commit date of FILE. Format is %Y-%m-%d."
  (with-temp-buffer
    (call-process "git" nil t nil "log" "--reverse" "--date=short" "--pretty=format:%cd" file)
    (goto-char (point-min))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun ethair/git-last-date (file)
  "Return the last commit date of FILE. Format is %Y-%m-%d."
  (with-output-to-string
    (with-current-buffer standard-output
      (call-process "git" nil t nil "log" "-1" "--date=short" "--pretty=format:%cd" file))))

(defun ethair/blog-preview (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((beg (+ 1 (re-search-forward "^#\\+BEGIN_PREVIEW$")))
          (end (progn (re-search-forward "^#\\+END_PREVIEW$")
                      (match-beginning 0))))
      (buffer-substring beg end))))

(defun ethair/org-html-format-spec (info)
  "Return format specification for preamble and postamble."
  (let* ((timestamp-format (plist-get info :html-metadata-timestamp-format))
         (file (plist-get info :input-file))
         (meta-date (org-export-data (org-export-get-date info timestamp-format)
                                     info))
         (creation-date (if (string= "" meta-date)
                            (ethair/git-first-date file)
                          meta-date))
         (last-update-date (ethair/git-last-date file)))
    `((?t . ,(org-export-data (plist-get info :title) info))
      (?s . ,(org-export-data (plist-get info :subtitle) info))
      (?d . ,creation-date)
      (?T . ,(format-time-string timestamp-format))
      (?a . ,(org-export-data (plist-get info :author) info))
      (?e . ,(mapconcat
              (lambda (e) (format "" e e))
              (split-string (plist-get info :email) ",+ *")
              ", "))
      (?c . ,(plist-get info :creator))
      (?C . ,last-update-date)
      (?v . ,(or (plist-get info :html-validation-link) ""))
      (?u . ,(if (string= creation-date last-update-date)
                 creation-date
               (format "%s (<a href=%s>Last update: %s</a>)"
                       creation-date
                       (format "%s/commits/master/%s" ethair/repository (file-relative-name file ethair/root))
                       last-update-date))))))
(advice-add 'org-html-format-spec :override 'ethair/org-html-format-spec)

(defun ethair/preamble (info)
  "Return preamble as a string."
  (let* ((file (plist-get info :input-file))
         (prefix (file-relative-name (expand-file-name "source" ethair/root)
                                     (file-name-directory file))))
    (format "<a href=\"%1$s/blog.html\">Ethair</a>" prefix)))

(setq org-html-postamble t
      org-html-postamble-format `(("en" ,(concat "<p>Ethair ©</p>
<p>All Rights Reserved - 2021</p>")))
      org-html-preamble #'ethair/preamble
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html-checkbox-type 'html
      org-html-html5-fancy t
      org-html-htmlize-output-type nil
      org-html-doctype "html5")

(defun ethair/org-publish-sitemap (title list)
  (setcdr list (seq-filter
                (lambda (file)
                  (string-match "file:[^ ]*/index.org" (car file)))
                (cdr list)))
  (concat "#+TITLE: " title "\n"
          "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">"
          "\n"
          ;; "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css\">"
          ;; "\n"
          "#+HTML_HEAD: <link href=\"atom.xml\" type=\"application/atom+xml\" rel=\"alternate\" title=\"Sterling's homepage\">"
          "\n"
          (org-list-to-org list)))

(defun ethair/org-publish-find-date (file project)
  (let ((file (org-publish--expand-file-name file project)))
    (or (org-publish-cache-get-file-property file :date nil t)
        (org-publish-cache-set-file-property
         file :date
         (let ((date (org-publish-find-property file :date project)))
           (let ((ts (and (consp date) (assq 'timestamp date))))
             (and ts
                  (let ((value (org-element-interpret-data ts)))
                    (and (org-string-nw-p value)
                         (org-time-string-to-time value))))))))))

(defun ethair/org-publish-sitemap-entry (entry style project)
  (cond ((not (directory-name-p entry))
         (let* ((meta-date (ethair/org-publish-find-date entry project))
                (file (expand-file-name entry
                                        (org-publish-property :base-directory project)))
                (creation-date (if (not meta-date)
                                   (ethair/git-first-date file)
                                 (format-time-string "%Y-%m-%d" meta-date)))
                (last-date (ethair/git-last-date file)))
;; %s (<a href=%s>Last update: %s</a>)
           (format "%s^{ (%s)} [[file:%s][%s]]"
                   (org-publish-find-title entry project)
                   (if (string= creation-date last-date)
                       creation-date
                     (format "%s, updated %s" creation-date last-date))
                   entry
                   "Read more →")))
        ((eq style 'tree)
         (capitalize (file-name-nondirectory (directory-file-name entry))))
        (t entry)))

(setq org-publish-project-alist
      (list
       (list "site-org"
             :base-directory "./source/"
             :recursive t
             :publishing-function '(org-html-publish-to-html)
             :publishing-directory "./public/"
             :sitemap-format-entry #'ethair/org-publish-sitemap-entry
             :auto-sitemap t
             :sitemap-title "Blog"
             :sitemap-filename "blog.org"
             :sitemap-style 'list
             :sitemap-function #'ethair/org-publish-sitemap
             :sitemap-sort-files 'anti-chronologically
						 :with-toc nil
             :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../style.css\">
<link href=\"../atom.xml\" type=\"application/atom+xml\" rel=\"alternate\" title=\"Ethair's homepage\">
")
;; <link rel=\"stylesheet\" type=\"text/css\" href=\"https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css\">
       (list "site-static"
             :base-directory "source/"
             :exclude "\\.org\\'"
             :base-extension 'any
             :publishing-directory "./public"
             :publishing-function 'org-publish-attachment
             :recursive t)
       (list "site-cert"
             :base-directory ".well-known"
             :exclude "public/"
             :base-extension 'any
             :publishing-directory "./public/.well-known"
             :publishing-function 'org-publish-attachment
             :recursive t)
       (list "site" :components '("site-org"))))

(defun ethair/publish ()
  (org-publish-all)
  (setq webfeeder-default-author "Ethair <boogs@ethair.com>")
  (webfeeder-build
   "rss.xml"
   "./public"
   "https://blog.ethair.com"
   (delete "index.html"
           (mapcar (lambda (f) (replace-regexp-in-string ".*/public/" "" f))
                   (directory-files-recursively "public" "index.html")))
   :builder 'webfeeder-make-rss
   :title "Ethair's homepage"
   :description "(Abstract Abstract)")
  (webfeeder-build
   "atom-xml"
   "./public"
   "https://blog.ethair.com"
   (delete "index.html"
           (mapcar (lambda (f) (replace-regexp-in-string ".*/public/" "" f))
                   (directory-files-recursively "public" "index.html")))
   :title "Ethair's homepage"
   :description "(Abstract Abstract)"))

(provide 'publish)
;; publish.el ends here
