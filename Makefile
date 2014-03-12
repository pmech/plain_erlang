
all: *.erl
	erl -make

%.erl : %.erl.md
	sed "s/^\S/%% /" $< > $@
