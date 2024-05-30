emacs ?= emacs
FILES := ialign.el
ELC := $(FILES:.el=.elc)

ARGS := --batch -Q

compile: $(ELC)

%.elc: %.el
	${emacs} ${ARGS} -L .                                                 \
		--eval '(setq byte-compile-error-on-warn t)'                  \
	 -f batch-byte-compile $<

lint:
	file=$$(mktemp)                                                       \
	&& ${emacs} -Q --batch ialign.el                                      \
		--eval '(checkdoc-file (buffer-file-name))' 2>&1 | tee $$file \
	&& test -z "$$(cat $$file)"                                           \
	&& (grep -n -E "^.{80,}" ialign.el `# Catch long lines`               \
	    | sed                                                             \
		-r 's/^([0-9]+).*/ialign.el:\1: Too long/;q1')

update-copyright-years:
	year=`date +%Y`;                                                      \
	sed -i *.el *.org -r                                                  \
	  -e 's/Copyright \(C\) ([0-9]+)(-[0-9]+)?/Copyright (C) \1-'$$year'/'

clean:
	rm -f *.elc
