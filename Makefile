unit-tests:
	cask exec ert-runner

integration-tests:
	-rm 2017-08-16-evil-org-mode-key-bindings-override-vim-bindings.meta
	-rm 2017-08-16-evil-org-mode-key-bindings-override-vim-bindings.org
	-rm 2017-08-16-letting-capslock-be-control-and-escape.meta
	-rm 2017-08-16-letting-capslock-be-control-and-escape.org
	cask exec emacs --batch --load export-org-journal-script.el 20170816.org

dev-install:
	cask install
