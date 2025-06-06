#!/bin/bash

ghdl -a --std=08 src/processor.vhd sim/testbench.vhd
ghdl -e --std=08 testbench
ghdl -r --std=08 testbench --fst=wave.fst --stop-time=400ns
