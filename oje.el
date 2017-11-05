;;; oje.el --- Support export of org-journal files to Nikola

;; Version: 0.0.0
;; Package-Requires: (f org seq)

(require 'org)
(require 'seq)

(require 'f)

(defun get-time(org-element)
  (let ((time (org-element-property :TIME org-element)))
    (if (not time)
        (setq time "00:00"))
    time))

(defun summarize()
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
  (let* ((file-name (file-name-base file-path)))
    (concat (substring file-name 0 4) "-"
            (substring file-name 4 6) "-"
            (substring file-name 6 8))
    )
  )

(defun extract-journal-entries(path-to-journal)
  (let ((date (file-name-base path-to-journal)))
    (seq-map (lambda (elt) (plist-put elt :date (extract-date date)))
             (seq-filter (lambda (elt) (eq 2 (plist-get elt :level)))
                         (org-map-entries 'summarize nil (list path-to-journal))))
    )
  )

(defun export-meta-info(journal-properties dest-dir)
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

(defun export-blog-content(journal-properties dest-dir)
  (let ((file-path (build-nikola-file-path journal-properties ".org" dest-dir)))
    (set-buffer (find-file-noselect file-path))
    (erase-buffer)
    (insert-string (concat "* " (plist-get journal-properties :title)))
    (newline)
    (insert-string (plist-get journal-properties :content))
    (save-buffer))
  )

(defun build-nikola-file-path(journal-properties extension dest-dir)
  (f-short (f-join dest-dir (build-file-name (plist-get journal-properties :date) (plist-get journal-properties :title) extension)))
  )

(defun build-file-name (date title extension)
  (concat date "-" (replace-regexp-in-string "[^a-z0-9_.~]" "-" (downcase title)) extension)
  )

(defun export-journal-entries(journal-file-path dest-dir)
  (let ((journal-entries (extract-journal-entries journal-file-path)))
    (mapc (lambda (entry) (export-meta-info entry dest-dir)) journal-entries)
    (mapc (lambda (entry) (export-blog-content entry dest-dir)) journal-entries)
    nil)
  )

(provide 'oje)

;;; oje.el ends here
