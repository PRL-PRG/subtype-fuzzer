# subtype-fuzzer — random type generator for testing Julia subtyping implementation

## Requirements

You need to have [Haskell `stack` tool](https://www.haskellstack.org).
For the Unix-like systems the installation can be done by one-liner:
    
    make get-stack

## Build

Although the project is `stack`-based, it has a 
simple `make`-interface. Consult `Makefile` for the main targets. 

To build the project:

    make

(It will simply call `stack build`.)


## Usage

To run the fuzzer to get some random types do:

    make run

It will generate `N`  groups of _similar_ types. The value of `N`
is hardcoded in the Haskell source (`src/Main.hs`, constant `exp_num`).
There are 99 of experiments by default.
