.PHONY: all fuzzer  run get-stack clean clean-all

all: fuzzer

fuzzer:
	stack build

run:
	stack exec fuzzer

get-stack:
	curl -sSL https://get.haskellstack.org/ | sh

clean:
	stack clean

clean-all: clean
	stack clean --full
