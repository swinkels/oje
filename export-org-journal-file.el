;;; export-org-journal-file.el --- Support export of org-journal files to Nikola

;; Version: 0.0.0

(require 'org)
(require 'seq)

(defun get-time(org-element)
  (let ((time (org-element-property :TIME org-element)))
    (if (not time)
        (setq time "00:00"))
    time))

(defun summarize()
  (let* ((element (org-element-at-point)))
    (let (properties)
           (setq properties (plist-put properties :level (org-element-property :level element)))
           (setq properties (plist-put properties :title (org-element-property :title element)))
           (setq properties (plist-put properties :time (get-time element)))
           (setq properties (plist-put properties :content
                                       (buffer-substring-no-properties
                                        (org-element-property :contents-begin element)
                                        (org-element-property :contents-end element))))
           )))

(defun build-file-name (date title extension)
  (concat date "-" (replace-regexp-in-string " " "-" (downcase title)) extension)
  )

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

(defun export-meta-info(journal-properties)
  (let* ((file-path (build-file-name (plist-get journal-properties :date) (plist-get journal-properties :title) ".meta"))
         (slug (file-name-base file-path)))
    (set-buffer (find-file-noselect file-path))
    (erase-buffer)
    (insert-string (concat ".. title: " (plist-get journal-properties :title)))
    (newline)
    (insert-string (concat ".. slug: " slug))
    (newline)
    (insert-string (concat ".. date: "
                           (plist-get journal-properties :date) " "
                           (plist-get journal-properties :time) " CET"))
    (save-buffer))
  )

(defun export-blog-content(journal-properties)
  (let ((file-path (build-file-name (plist-get journal-properties :date) (plist-get journal-properties :title) ".org")))
    (set-buffer (find-file-noselect file-path))
    (erase-buffer)
    (insert-string (concat "* " (plist-get journal-properties :title)))
    (newline)
    (insert-string (plist-get journal-properties :content))
    (save-buffer))
  )

(defun export-journal-entries(journal-file-path)
  (let ((journal-entries (extract-journal-entries journal-file-path)))
    (mapc 'export-meta-info journal-entries)
    (mapc 'export-blog-content journal-entries)
    nil)
  )

(provide 'export-org-journal-file)

;;; export-org-journal-file.el ends here
