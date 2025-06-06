library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.pack.all;

entity testbench is
end entity;

architecture Mixed of testbench is
    -- Clock
    signal Clock : std_logic;
    constant ClockPeriod : time := 10 ns;

    -- RAM
    component tb_ram is
        port(
            clka    : in  std_logic;
            wea     : in  std_logic;
            addra   : in  word_t;
            dina    : in  word_t;
            douta   : out word_t);
    end component;

    -- UUT
    signal Reset, Run : std_logic;
    signal MemoryEnable, WriteEnable : std_logic;
    signal Address : word_t;
    signal DIn : word_t;
    signal DOut : word_t;
begin
    -- Clock
    process
    begin
        Clock <= '0';

        loop
            wait for ClockPeriod / 2;
            Clock <= not Clock;
        end loop;
    end process;

    -- RAM
    RAM : component tb_ram port map(Clock, WriteEnable, Address, DOut, DIn);

    -- UUT
    UUT : component processor port map(
        Reset => Reset,
        Clock => Clock,
        Run => Run,
        MemoryEnable => MemoryEnable,
        WriteEnable => WriteEnable,
        Address => Address,
        DIn => DIn,
        DOut => DOut
    );

    -- Stimulus
    process
    begin
        Reset <= '1';
        Run <= '0';
        wait until falling_edge(Clock);
        wait until falling_edge(Clock);
        wait until falling_edge(Clock);

        Reset <= '0';
        Run <= '1';
        wait;
    end process;
end;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.pack.all;

entity tb_ram is
    port(
        clka    : in  std_logic;
        wea     : in  std_logic;
        addra   : in  word_t;
        dina    : in  word_t;
        douta   : out word_t);
end;

architecture behavioral of tb_ram is
    type ram_type is array (0 to 255) of word_t;
    signal ram : ram_type := (
        0 => "00000000000000000000000000000111", -- reg[0] = 7
        1 => "00000000001000000000010000001101", -- reg[1] = 13 * 65536
        2 => "00100000000000000000100000000001", -- reg[2] = reg[0] + reg[1]
        3 => "11100000000000000000000000000000", -- dump reg[0]
        4 => "11100000000000000000010000000000", -- dump reg[1]
        5 => "11100000000000000000100000000000", -- dump reg[2]
        others => (others => 'U')
    );

    signal read_addr : word_t;
begin
    process
    begin
        wait until rising_edge(clka);

        if wea = '1' then
            ram(to_integer(unsigned(addra))) <= dina;
        end if;

        read_addr <= addra;
    end process;

    douta <= ram(to_integer(unsigned(read_addr)));
end;
