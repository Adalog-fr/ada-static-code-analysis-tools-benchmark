.PHONY: lib tests pretty

lib:
	alr build

tests:
	alr exec -- gprbuild -Ptests

pretty:
	gnatpp src/*.ad? tests/*.ad?
