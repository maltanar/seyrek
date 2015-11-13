# seyrek
A framework for generating semiring-based sparse graph accelerators for FPGAs. Currently under heavy development, more documentation will be added as the project progresses.

## Overview
The core idea is to use the matrices-over-semirings as a building block for implementing graph algorithms. Here is a [presentation by Aydın Buluç (LBNL)](http://gauss.cs.ucsb.edu/~aydin/ex14_graph_blas.pdf) that explains the concept. Essentially, given a semiring definition (and several other parameters) in Chisel, Seyrek builds high-performance FPGA accelerators that implement sparse linear algebra primitives on that semiring.

## Primitives

So far, support for two sparse linear algebra primitives is planned:
* sparse matrix times dense vector (SpMV)
* sparse matrix times sparse vector

## Platforms

Seyrek builds on the [fpga-tidbits](https://github.com/maltanar/fpga-tidbits) framework to provide support for different platforms, so accelerators can be generated and run on different FPGA boards as long as there is a platform wrapper defined in fpga-tidbits.




