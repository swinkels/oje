(require 'org)
(require 'seq)

(defun summarize()
  (let* ((element (org-element-at-point)))
    (let (properties)
           (setq properties (plist-put properties :level (org-element-property :level element)))
           (setq properties (plist-put properties :title (org-element-property :title element)))
           (setq properties (plist-put properties :content
                                       (buffer-substring-no-properties
                                        (org-element-property :contents-begin element)
                                        (org-element-property :contents-end element))))
           )))

(seq-map (lambda (elt) (print (plist-get elt :title)))
         (seq-filter (lambda (elt) (eq 2 (plist-get elt :level)))
                     (org-map-entries 'summarize nil (list "/home/pieter/repos/local/export-org-journal/20170816.org"))))
