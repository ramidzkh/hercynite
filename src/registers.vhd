library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.pack.all;

entity RegisterFile is
    port (
        clk   : in std_logic;
        reset : in std_logic;
        WrEn  : in std_logic;
        R1, R2, Wr : in reg_select_t;
        WrIn : in word_t;
        R1Out, R2Out : out word_t
    );
end entity;

architecture rtl of RegisterFile is
    type registers_array is array (0 to 31) of word_t;
    signal R : registers_array;
begin
    process (clk, reset)
    begin
        if reset = '1' then
            R <= (others => word_0);
        elsif rising_edge(clk) and WrEn = '1' then
            for i in R'range loop
                if to_integer(unsigned(Wr)) = i then
                    R(i) <= WrIn;
                end if;
            end loop;
        end if;
    end process;

    process (all)
    begin
        R1Out <= (others => '-');

        for i in R'range loop
            if to_integer(unsigned(R1)) = i then
                R1Out <= R(i);
            end if;
        end loop;
    end process;

    process (all)
    begin
        R2Out <= (others => '-');

        for i in R'range loop
            if to_integer(unsigned(R2)) = i then
                R2Out <= R(i);
            end if;
        end loop;
    end process;
end architecture;
