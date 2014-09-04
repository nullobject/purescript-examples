sources    = $(wildcard Example*.purs)
objects    = $(patsubst %.purs, build/%.js, $(sources))
examples   = $(patsubst %.purs, %, $(sources))
components = $(shell find bower_components -type f -name '*.purs')

.PHONY: all clean

all: bower_components $(objects)

clean:
	@rm -rf bower_components build

bower_components:
	@bower install

%: build/%.js
	@node $<

build/%.js: module = $(notdir $(basename $@))
build/%.js: %.purs
	@mkdir -p build
	psc --module=$(module) --main=$(module) --output=$@ $(components) $<
