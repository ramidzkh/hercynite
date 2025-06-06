# Create and configure project
set proj_name processor
set proj_dir [file normalize "./vivado-project"]
create_project $proj_name $proj_dir -force
set_property target_language VHDL [current_project]

add_files -norecurse {
    ./src/processor.vhd
}

add_files -fileset sim_1 -norecurse {
    ./sim/testbench.vhd
    ./sim/testbench_behav.wcfg
}

set_property -name {xsim.simulate.runtime} -value {400ns} -objects [get_filesets sim_1]

set_property top testbench [get_filesets sim_1]
set_property top_lib xil_defaultlib [get_filesets sim_1]
set_property file_type {VHDL 2008} [get_files *.vhd]
