;;; test-helper.el --- Helpers for export-org-journal-test.el

(require 'f)

;; copied from http://rejeep.github.io/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html

(defvar root-test-path
  (f-dirname (f-this-file)))

(defvar root-code-path
  (f-parent root-test-path))

(require 'export-org-journal-file
         (f-expand "export-org-journal-file.el" root-code-path))

;;; test-helper.el ends here
