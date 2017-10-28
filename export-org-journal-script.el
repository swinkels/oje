(load "/home/pieter/repos/github.com/oje/export-org-journal-file.el")

(require 'f)

(let ((dest-dir "/home/pieter/repos/github.com/nikola-journal/posts")
      (journal-files
       (f-files org-journal-dir (lambda (file) (equal (f-ext file) "org")))))
  (mapc (lambda (journal-file) (export-journal-entries journal-file dest-dir))
        journal-files))
