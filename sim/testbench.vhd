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
    signal Reset, Run, Frozen : std_logic;
    signal MemoryEnable, WriteEnable : std_logic;
    signal Address : word_t;
    signal DIn : word_t;
    signal DOut : word_t;
begin
    -- Clock
    process
    begin
        Clock <= '0';

        while Frozen /= '1' loop
            wait for ClockPeriod / 2;
            Clock <= not Clock;
        end loop;

        report "UUT reported frozen";

        -- extra cycles
        for i in 1 to 10 loop
            wait for ClockPeriod / 2;
            Clock <= not Clock;
        end loop;

        wait;
    end process;

    -- RAM
    RAM : component tb_ram port map(Clock, WriteEnable, Address, DOut, DIn);

    -- UUT
    UUT : component processor port map(
        Reset => Reset,
        Clock => Clock,
        Run => Run,
        Frozen => Frozen,
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
use STD.textio.all;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.std_logic_textio.all;
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
    type ram_type is array (0 to 63) of word_t;
    signal ram : ram_type := (others => (others => 'U'));

    signal read_addr : word_t;
begin
    process
        file SOURCE : text;
        variable myLine : line;
        variable lineVal : word_t;
        variable i : integer := 0;
    begin
        FILE_OPEN(SOURCE, "/dev/stdin", READ_MODE);

        while not ENDFILE(SOURCE) loop
            readline(SOURCE, myLine);
            read(myLine, lineVal);

            ram(i) <= lineVal;
            i := i + 1;
        end loop;

        FILE_CLOSE(SOURCE);

        loop
            wait until rising_edge(clka);

            if wea = '1' then
                ram(to_integer(unsigned(addra))) <= dina;
            end if;

            read_addr <= addra;
        end loop;
    end process;

    douta <= ram(to_integer(unsigned(read_addr)));
end;
