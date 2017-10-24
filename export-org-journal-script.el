(load "/home/pieter/repos/local/export-org-journal/export-org-journal-file.el")

;; (require 'f)

;; (f-files org-journal-dir (lambda (file) (equal (f-ext file) "org")))

(let ((dest-dir "."))
  (mapc
   (lambda (journal-file) (export-journal-entries journal-file dest-dir)) argv)
  )
