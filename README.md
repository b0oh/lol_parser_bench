# Lol parser benchmark

There three parsers:
* lol_parser
* comb parser uses small parse combinator library (pc)
  without line numbers and error handling
* erl_tools parser uses leex and yecc

## Build and run

Note: it may take a long time

```
$ erl -make
$ erl
> erl_tools:compile_tools().
> bench:run().
```

## Results

On my machine (1.7GHz i7, 8 GB):

```
lol_parse:
Range: 39306 - 95514 mics
Median: 49652 mics
Average: 51566 mics

comb:
Range: 138005 - 254776 mics
Median: 154961 mics
Average: 156792 mics

erl_tools:
Range: 302036 - 620466 mics
Median: 382961 mics
Average: 390783 mics
```
