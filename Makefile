emacs ?= emacs
FILES := ialign.el
ELC := $(FILES:.el=.elc)

ARGS := --batch -Q

compile: $(ELC)

%.elc: %.el
	${emacs} ${ARGS} -L . -f batch-byte-compile $<

update-copyright-years:
	year=`date +%Y`;                                                      \
	sed -i *.el *.org -r                                                  \
	  -e 's/Copyright \(C\) ([0-9]+)(-[0-9]+)?/Copyright (C) \1-'$$year'/'

clean:
	rm -f *.elc
