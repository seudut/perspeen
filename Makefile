emacs ?= emacs

LOAD = -l perspeen-tab.el -l perspeen.el

INIT_PACKAGE_EL = "(progn \
  (require 'package) \
  (push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives) \
  (package-initialize))"


all:compile check

compile:
	$(emacs) -batch -l target/perspeen-init.el
run:
	$(emacs) -Q -l target/perspeen-init.el
check:
	$(emacs) -Q -batch \
		--eval $(INIT_PACKAGE_EL) \
		--eval '(package-refresh-contents)' \
		--eval "(unless (package-installed-p 'package-lint) (package-install 'package-lint))" \
		-f package-lint-batch-and-exit \
		perspeen-tab.el perspeen.el
