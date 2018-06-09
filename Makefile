.PHONY: rj

JULIA_CMD=julia-dev-raw --depwarn=no
RJ=stack exec rj
SRC=src

all: rj

rj:
	stack build

# > out.txt && cat out.txt
run: rj
	$(RJ) | $(JULIA_CMD) -L $(SRC)/check.jl -e 'main()'

runin:
	$(RJ) > out.txt

runout:
	cat out.txt | $(JULIA_CMD) -L $(SRC)check.jl -e 'main()'

get-stack:
	curl -sSL https://get.haskellstack.org/ | sh

clean:
	stack clean

clean-all: clean
	stack clean --full

