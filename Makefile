
SRC_MD := $(shell find $(SOURCEDIR) -name '*.erl.md')

SRC_ERL=$(SRC_MD:%.erl.md=%.erl)

all: $(SRC_ERL)
	erl -make

%.erl : %.erl.md
	sed "s/^\S/%% /" $< > $@

clean:
	rm *.erl *.beam
