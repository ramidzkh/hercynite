#!/bin/bash

set -ex

rm work-obj08.cf || true
ghdl -a --std=08 -Wall src/processor.vhd sim/testbench.vhd
ghdl -e --std=08 -Wall testbench
ghdl -r --std=08 -Wall testbench --fst=wave.fst --stop-time=6500ns
