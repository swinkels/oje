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
