build:
	hpack
	stack build

run:
	stack run

build-run: build run
