# Hercynite

A dead simple CPU and ISA

## Development

If you have Vivado, the `generate-vivado` tcl scripts should generate a project.

If you want to use GHDL instead, the `ghdl.sh` script should run the simulation and generate a `wave.fst` you can load in GTKWave.
This setup is at least 200 times lighter and faster.

In VSCode, I recommend the `hbohlin.vhdl-ls` extension and using the provided tasks to run GHDL and GTKWave.
