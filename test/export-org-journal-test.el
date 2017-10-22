;;; export-org-journal-test.el --- Tests for export-org-journal

(require 'export-org-journal-file)

(ert-deftest extract-date-from-file-name()
  (should (equal (extract-date "20170816.org") "2017-08-16"))
  )

(ert-deftest extract-date-from-file-path()
  (should (equal (extract-date  "/home/pieter/repos/local/export-org-journal/20170816.org") "2017-08-16"))
  )

;;; export-org-journal-test.el ends here
