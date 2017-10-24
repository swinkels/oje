;;; export-org-journal-test.el --- Tests for export-org-journal

(require 'export-org-journal-file)

(ert-deftest extract-date-from-file-name()
  (should (equal (extract-date "20170816.org") "2017-08-16"))
  )

(ert-deftest extract-date-from-file-path()
  (should (equal (extract-date  "/home/pieter/repos/local/export-org-journal/20170816.org") "2017-08-16"))
  )

(ert-deftest build-nikola-file-path-to-content()
  (let ((properties '(:date "20171022" :title "Hello world")))
    (should (equal (build-nikola-file-path properties ".org" "~/tmp") "~/tmp/20171022-hello-world.org"))))

(ert-deftest build-nikola-file-path-to-meta-info()
  (let ((properties '(:date "20171022" :title "Hello world")))
    (should (equal (build-nikola-file-path properties ".meta" "~/tmp") "~/tmp/20171022-hello-world.meta"))))

;;; export-org-journal-test.el ends here
