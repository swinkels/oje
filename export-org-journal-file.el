(require 'org)

(defun summarize()
  (let* ((element (org-element-at-point)))
    (print (nth 1 element)) ;;(org-element-property :heading element))
    ))

(defun summarize()
  (let* ((element (org-element-at-point)))
    (print (org-element-property :level element))
    (print (buffer-substring-no-properties
            (org-element-property :contents-begin element)
            (org-element-property :contents-end element)))
    (print (org-element-property :title element))
    ))

(org-map-entries 'summarize nil (list "/home/pieter/repos/local/export-org-journal/20170816.org"))

(let ((properties '(:title "Hello World!" :date "20170816" :time "10:24" :content "This is a story about the world")))
  (plist-get properties ':title)
  )
