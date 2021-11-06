Hermes hardware generation
=======================

Contents at a glance:

* `build.sc`: the Mill build script.
* `Makefile`: various useful shorthands for mill targets.
* `build`: the default output directory for the `verilog` target in the Makefile.
* `out`: output directory for mill targets.
* `test_run_dir`: runtime output for unit tests.  Contains simulation waveforms.
* `playground`: all sources and tests.

## Getting Started

First, install mill by referring to the documentation [here](https://com-lihaoyi.github.io/mill).

To run all tests in this design (recommended for test-driven development):
```bash
make test # to run all tests
make test TEST=<TestName> # to run a single test set
```

To generate Verilog with the `Elaborate` entry point:
```bash
make verilog
```

To generate BSP connection files for use with an IDE (e.g. IntelliJ IDEA):
```bash
make bsp
```
