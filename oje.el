;;; oje.el --- Support export of org-journal files to Nikola

;; Version: 0.0.0
;; Package-Requires: (f org seq)

(require 'org)
(require 'seq)

(require 'f)

(defun get-time(org-element)
  "Return the value (as a string) of the :TIME property of
ORG-ELEMENT. If ORG-ELEMENT does not have this property, this
function returns 00:00."
  (let ((time (org-element-property :TIME org-element)))
    (if (not time)
        (setq time "00:00"))
    time))

(defun summarize()
  "Return a plist with the information of the
org-element-at-point."
  (let* ((element (org-element-at-point)))
    (let (properties)
           (setq properties (plist-put properties :level (org-element-property :level element)))
           (setq properties (plist-put properties :tags (org-element-property :tags element)))
           (setq properties (plist-put properties :title (org-element-property :title element)))
           (setq properties (plist-put properties :time (get-time element)))
           (setq properties (plist-put properties :content
                                       (buffer-substring-no-properties
                                        (org-element-property :contents-begin element)
                                        (org-element-property :contents-end element))))
           )))

(defun extract-date(file-path)
  "Return the date (as a string) specified by the file name of
FILE-PATH. This function assumes this file name has format
YYYYMMDD."
  (let* ((file-name (file-name-base file-path)))
    (concat (substring file-name 0 4) "-"
            (substring file-name 4 6) "-"
            (substring file-name 6 8))
    )
  )

(defun oje--extract-journal-entries(journal-file-path)
  "Return a list of plists that each specifies an entry of
org-journal file JOURNAL-FILE-PATH. The properties of the plist
are :date and the ones that are extracted by function summarize.

This function only extracts Org section of level 2: it assumes
that each such section is a separate journal entry."
  (let ((date (file-name-base journal-file-path)))
    (seq-map (lambda (elt) (plist-put elt :date (extract-date date)))
             (seq-filter (lambda (elt) (eq 2 (plist-get elt :level)))
                         (org-map-entries 'summarize nil (list journal-file-path))))
    )
  )

(defun oje--export-meta-info(journal-properties dest-dir)
  "Export JOURNAL-PROPERTIES to a .meta file in DEST-DIR.
JOURNAL-PROPERTIES is a plist that specifies a single entry of an
org-journal file."
  (let* ((file-path (build-nikola-file-path journal-properties ".meta" dest-dir))
         (slug (file-name-base file-path))
         (tags (mapconcat 'identity (plist-get journal-properties :tags) ", ")))
    (set-buffer (find-file-noselect file-path))
    (erase-buffer)
    (insert-string (concat ".. title: " (plist-get journal-properties :title)))
    (newline)
    (insert-string (concat ".. slug: " slug))
    (newline)
    (insert-string (concat ".. date: "
                           (plist-get journal-properties :date) " "
                           (plist-get journal-properties :time) " CET"))
    (newline)
    (when tags
      (insert-string (concat ".. tags: " tags))
      (newline))
    (save-buffer))
  )

(defun oje--export-blog-content(journal-properties dest-dir)
  "Export JOURNAL-PROPERTIES to a .org file in DEST-DIR.
JOURNAL-PROPERTIES is a plist that specifies a single entry of an
org-journal file.
"
  (let ((file-path (build-nikola-file-path journal-properties ".org" dest-dir)))
    (set-buffer (find-file-noselect file-path))
    (erase-buffer)
    (insert-string (plist-get journal-properties :content))
    (save-buffer))
  )

(defun build-nikola-file-path(journal-properties extension dest-dir)
  (f-short (f-join dest-dir (build-file-name (plist-get journal-properties :date) (plist-get journal-properties :title) extension)))
  )

(defun build-file-name (date title extension)
  (concat date "-" (replace-regexp-in-string "[^a-z0-9_.~]" "-" (downcase title)) extension)
  )

(defun oje-export-journal-file(journal-file-path dest-dir)
  "Export all entries of org-journal file JOURNAL-FILE-PATH to
DEST-DIR. Each entry of the org-journal file is exported to two
files, namely an .org file that contains the text of the entry
and a .meta file that contains meta information such as title,
date and time, tags and slug."
  (let ((journal-entries (oje--extract-journal-entries journal-file-path)))
    (mapc (lambda (entry) (oje--export-meta-info entry dest-dir)) journal-entries)
    (mapc (lambda (entry) (oje--export-blog-content entry dest-dir)) journal-entries)
    nil)
  )

(provide 'oje)

;;; oje.el ends here
