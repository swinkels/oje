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

(seq-map (lambda (elt) (print (plist-get elt :title)))
         (seq-filter (lambda (elt) (eq 2 (plist-get elt :level)))
                     (org-map-entries 'summarize nil (list "/home/pieter/repos/local/export-org-journal/20170816.org"))))

(defun build-filename (date title extension)
  (concat date "-" (replace-regexp-in-string " " "-" (downcase title)) extension)
  )

(build-filename "20170816" "Letting CapsLock be Control and Escape" ".org")
(build-filename "20170816" "Letting CapsLock be Control and Escape" ".meta")

(org-map-entries 'org-element-at-point nil (list "/home/pieter/repos/local/export-org-journal/20170816.org"))

(seq-filter (lambda (elt) (eq 2 (plist-get elt :level)))
            (org-map-entries 'summarize nil (list "/home/pieter/repos/local/export-org-journal/20170816.org")))

(defun extract-date(filepath)
  (let* ((file-name (file-name-base filepath)))
    (concat (substring file-name 0 4) "-"
            (substring file-name 4 6) "-"
            (substring file-name 6 8))
    )
  )

(ert-deftest extract-date-from-filename()
  (should (equal (extract-date "20170816.org") "2017-08-16"))
  )

(ert-deftest extract-date-from-filepath()
  (should (equal (extract-date  "/home/pieter/repos/local/export-org-journal/20170816.org") "2017-08-16"))
  )

(defun extract-journal-properties(filepath)
  (let* ((date (file-name-base filepath)))
    (seq-map (lambda (elt) (plist-put elt :date (extract-date date)))
             (seq-filter (lambda (elt) (eq 2 (plist-get elt :level)))
                         (org-map-entries 'summarize nil (list filepath))))
    )
  )

(defun export-meta-info(journal-properties)
  (let* ((file-path (build-filename (plist-get journal-properties :date) (plist-get journal-properties :title) ".meta"))
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
  (let ((file-path (build-filename (plist-get journal-properties :date) (plist-get journal-properties :title) ".org")))
    (set-buffer (find-file-noselect file-path))
    (erase-buffer)
    (insert-string (concat "* " (plist-get journal-properties :title)))
    (newline)
    (insert-string (plist-get journal-properties :content))
    (save-buffer))
  )

(defun export-org-journals(filepath)
  (let ((journal-properties (extract-journal-properties filepath)))
    (mapc 'export-meta-info journal-properties)
    (mapc 'export-blog-content journal-properties)
    nil)
  )

(export-org-journals  "/home/pieter/repos/local/export-org-journal/20170816.org")
