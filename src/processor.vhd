library IEEE;
use IEEE.std_logic_1164.all;

package pack is
    subtype word_t is std_logic_vector(31 downto 0);

    component processor is
        port(
            -- General
            Reset, Clock, Run : in std_logic;
            -- Memory
            MemoryEnable, WriteEnable : out std_logic;
            Address : out word_t;
            DIn : in word_t;
            DOut : out word_t);
    end component;

    component regn is
        generic(N : natural := 32);
        port(
            Reset, Clock, Enable : in std_logic;
            R : out std_logic_vector(N - 1 downto 0);
            Rin : in std_logic_vector(N - 1 downto 0));
    end component;
end package;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.pack.all;

entity processor is
    port(
        -- General
        Reset, Clock, Run : in std_logic;
        -- Memory
        MemoryEnable, WriteEnable : out std_logic;
        Address : out word_t;
        DIn : in word_t;
        DOut : out word_t);
end;

architecture Mixed of processor is
    -- the top 3 bits determines what class of instruction

    -- 000xxxxx xxxIIIII IxxxxxII IIIIIIII: A type (16 bit immediate)
    -- 00000000 000IIIII IaaaaaII IIIIIIII: reg[a] = imm
    -- 00000000 001IIIII IaaaaaII IIIIIIII: reg[a] = imm << 16

    -- 001xxxxx xxxxxxxx xaaaaabb bbbccccc: B type (up to 3 registers)
    -- 00100000 00000000 0aaaaabb bbbccccc: reg[a] = reg[b] + reg[c]
    -- 00100000 00000000 1aaaaabb bbbccccc: reg[a] = reg[b] - reg[c]
    -- 00100001 00000000 0aaaaabb bbbccccc: reg[a] = reg[b] | reg[c]
    -- 00100001 00000000 1aaaaabb bbbccccc: reg[a] = reg[b] & reg[c]
    -- 00100010 00000000 0aaaaabb bbbxxxxx: reg[a] = memory[reg[b]]
    -- 00100010 00000000 1aaaaabb bbbxxxxx: memory[reg[a]] = reg[b]

    -- 100xxxxx xxxxxxxx xxxxxxxx xxxxxxxx: E type (CPU state)
    -- 10000000 00000000 00000000 000ccccc: reg[c] = IP
    -- 10000001 00000000 00000000 000ccccc: IP = reg[c]

    -- 111xxxxx xxxxxxxx xxxxxxxx xxxxxxxx: X type (development/simulation)
    -- 11100000 00000000 0aaaaa00 00000000: Dump reg[a]
    -- 11111111 11111111 11111111 11111111: Dump full processor state

    -- general purpose registers
    type registers_array is array(0 to 31) of word_t;
    signal R, RNext : registers_array;

    -- internal registers
    signal IP, IPNext, Instruction : word_t;

    -- decoded instruction
    signal IRA, IRB, IRC : std_logic_vector(4 downto 0);
    signal IImm : std_logic_vector(15 downto 0);

    type StateType is (WaitForRun, InsLoadSetup, InsLoadHold, InsLoadDone, Blehehehe, MemLoadHold, MemLoadDone, MemStoreHold);
    signal current_state, next_state : StateType;
begin
    -- decoded instruction
    IRA <= Instruction(14 downto 10);
    IRB <= Instruction(9 downto 5);
    IRC <= Instruction(4 downto 0);
    IImm <= Instruction(20 downto 15) & Instruction(9 downto 0);

    process(Reset, Clock)
    begin
        if Reset = '1' then
            current_state <= WaitForRun;
            R <= (others => (others => '0'));
            IP <= (others => '0');
        elsif rising_edge(Clock) then
            current_state <= next_state;
            R <= RNext;
            IP <= IPNext;
        end if;
    end process;

    process(all)
        function to_string(slv : std_logic_vector) return string is
            variable result : string(slv'range);
        begin
            for i in slv'range loop
                result(i) := character'VALUE(std_ulogic'image(slv(i)));
            end loop;
            return result;
        end;
    begin
        RNext <= R;
        IPNext <= IP;
        MemoryEnable <= '0';
        WriteEnable <= '0';

        case current_state is
            when WaitForRun =>
                if Run = '1' then
                    next_state <= InsLoadSetup;
                else
                    next_state <= WaitForRun;
                end if;
            when InsLoadSetup =>
                MemoryEnable <= '1';
                Address <= IP;
                next_state <= InsLoadHold;
            when InsLoadHold =>
                MemoryEnable <= '1';
                Address <= IP;
                next_state <= InsLoadDone;
            when InsLoadDone =>
                Instruction <= DIn;
                IPNext <= word_t(unsigned(IP) + 1);
                next_state <= Blehehehe;
            when Blehehehe =>
                report "insn: " & to_string(Instruction);

                if false then
                    -- formatting
                elsif std_match("00000000000---------------------", Instruction) then
                    -- reg[a] = imm
                    RNext(to_integer(unsigned(IRA))) <= "0000000000000000" & IImm;
                    next_state <= WaitForRun;
                elsif std_match("00000000001---------------------", Instruction) then
                    -- reg[a] = imm << 16
                    RNext(to_integer(unsigned(IRA))) <= IImm & "0000000000000000";
                    next_state <= WaitForRun;
                elsif std_match("00100000000000000---------------", Instruction) then
                    -- reg[a] = reg[b] + reg[c]
                    RNext(to_integer(unsigned(IRA))) <= word_t(unsigned(R(to_integer(unsigned(IRB)))) + unsigned(R(to_integer(unsigned(IRC)))));
                    next_state <= WaitForRun;
                elsif std_match("00100000000000001---------------", Instruction) then
                    -- reg[a] = reg[b] - reg[c]
                    RNext(to_integer(unsigned(IRA))) <= word_t(unsigned(R(to_integer(unsigned(IRB)))) - unsigned(R(to_integer(unsigned(IRC)))));
                    next_state <= WaitForRun;
                elsif std_match("00100001000000000---------------", Instruction) then
                    -- reg[a] = reg[b] | reg[c]
                    RNext(to_integer(unsigned(IRA))) <= R(to_integer(unsigned(IRB))) or R(to_integer(unsigned(IRC)));
                    next_state <= WaitForRun;
                elsif std_match("00100001000000001---------------", Instruction) then
                    -- reg[a] = reg[b] & reg[c]
                    RNext(to_integer(unsigned(IRA))) <= R(to_integer(unsigned(IRB))) and R(to_integer(unsigned(IRC)));
                    next_state <= WaitForRun;
                elsif std_match("00100010000000000---------------", Instruction) then
                    -- reg[a] = memory[reg[b]]
                    MemoryEnable <= '1';
                    WriteEnable <= '0';
                    Address <= R(to_integer(unsigned(IRB)));
                    next_state <= MemLoadHold;
                elsif std_match("00100010000000001---------------", Instruction) then
                    -- memory[reg[a]] = reg[b]
                    MemoryEnable <= '1';
                    WriteEnable <= '1';
                    Address <= R(to_integer(unsigned(IRA)));
                    DOut <= R(to_integer(unsigned(IRB)));
                    next_state <= MemStoreHold;
                elsif std_match("100000000000000000000000000-----", Instruction) then
                    -- reg[c] = IP
                    RNext(to_integer(unsigned(IRC))) <= IP;
                    next_state <= WaitForRun;
                elsif std_match("100000010000000000000000000-----", Instruction) then
                    -- IP = reg[c]
                    IPNext <= R(to_integer(unsigned(IRC)));
                    next_state <= WaitForRun;
                elsif std_match("11100000000000000-----0000000000", Instruction) then
                    -- Dump reg[a]
                    report "Register " & integer'image(to_integer(unsigned(IRA))) & " has value " & to_string(R(to_integer(unsigned(IRA))));
                    next_state <= WaitForRun;
                else
                    -- Dump full processor state
                    report "todo dump state";

                    if std_match("11111111111111111111111111111111", Instruction) then
                        next_state <= WaitForRun;
                    else
                        report "illegal instruction";
                        next_state <= current_state;
                    end if;
                end if;
            when MemLoadHold =>
                MemoryEnable <= '1';
                Address <= R(to_integer(unsigned(IRB)));
                next_state <= MemLoadDone;
            when MemLoadDone =>
                RNext(to_integer(unsigned(IRA))) <= DIn;
                next_state <= WaitForRun;
            when MemStoreHold =>
                MemoryEnable <= '1';
                WriteEnable <= '1';
                Address <= R(to_integer(unsigned(IRA)));
                DOut <= R(to_integer(unsigned(IRB)));
                next_state <= WaitForRun;
        end case;
    end process;
end;

library IEEE;
use IEEE.std_logic_1164.all;

entity regn is
    generic(N : natural := 32);
    port(
        Reset, Clock, Enable : in std_logic;
        R : out std_logic_vector(N - 1 downto 0);
        Rin : in std_logic_vector(N - 1 downto 0));
end;

architecture Behavior of regn is
begin
    process(Reset, Clock)
    begin
        if Reset = '1' then
            R <= (others => '0');
        elsif rising_edge(Clock) and Enable = '1' then
            R <= Rin;
        end if;
    end process;
end;
