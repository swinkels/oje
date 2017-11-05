;; This snippet shows how to use oje to export all org-journal entries in your
;; org-journal-dir directory. It assumes package oje has been loaded.

(require 'f)
(require 'oje)

(let ((dest-dir "/home/pieter/repos/github.com/nikola-journal/posts")
      (journal-files
       (f-files org-journal-dir (lambda (file) (equal (f-ext file) "org")))))
  (mapc (lambda (journal-file) (export-journal-entries journal-file dest-dir))
        journal-files))
