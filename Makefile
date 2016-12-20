emacs ?= emacs

LOAD = -l perspeen-tab.el -l perspeen.el

all:compile

compile:
	$(emacs) -batch $(LOAD) -l targets/perspeen-init.el
