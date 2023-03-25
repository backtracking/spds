
all:
	dune build
	time dune runtest

bench:
	dune exec ./bench.exe

build:
	dune build

test:
	dune runtest

doc:
	dune build @doc

clean:
	dune clean
