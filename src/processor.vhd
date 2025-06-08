library IEEE;
use IEEE.std_logic_1164.all;

package pack is
    subtype word_t is std_logic_vector(31 downto 0);
    subtype reg_select_t is std_logic_vector(4 downto 0);
    subtype imm16 is std_logic_vector(15 downto 0);

    constant word_0 : word_t := (others => '0');

    component processor is
        port(
            -- General
            Reset, Clock, Run : in std_logic;
            Frozen : out std_logic;
            -- Memory
            MemoryEnable, WriteEnable : out std_logic;
            Address : out word_t;
            DIn : in word_t;
            DOut : out word_t);
    end component;

    component RegisterFile is
        port (
            clk   : in std_logic;
            reset : in std_logic;
            WrEn  : in std_logic;
            R1, R2, Wr : in reg_select_t;
            WrIn : in word_t;
            R1Out, R2Out : out word_t
        );
    end component;

    component regn is
        generic(N : natural := 32);
        port(
            Reset, Clock, Enable : in std_logic;
            R : out std_logic_vector(N - 1 downto 0);
            Rin : in std_logic_vector(N - 1 downto 0));
    end component;

    component alu is
        port(
            A, B : in word_t;
            C : out word_t;
            Op : in std_logic_vector(4 downto 0);
            UnknownOp : out std_logic);
    end component;

    pure function zext_imm(imm: imm16) return word_t;
    pure function sext_imm(imm: imm16) return word_t;
    pure function push_imm(imm: imm16) return word_t;
end package;

library IEEE;
use IEEE.numeric_std.all;

package body pack is
    pure function zext_imm(imm: imm16) return word_t is
    begin
        return word_t(resize(unsigned(imm), 32));
    end;

    pure function sext_imm(imm: imm16) return word_t is
    begin
        return word_t(resize(signed(imm), 32));
    end;

    pure function push_imm(imm: imm16) return word_t is
    begin
        return imm & (15 downto 0 => '0');
    end;
end package body;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.pack.all;

entity processor is
    port(
        -- General
        Reset, Clock, Run : in std_logic;
        Frozen : out std_logic;
        -- Memory
        MemoryEnable, WriteEnable : out std_logic;
        Address : out word_t;
        DIn : in word_t;
        DOut : out word_t);
end;

architecture Mixed of processor is
    -- the top 3 bits determines what class of instruction

    --  3 2 2 2  2 2 1 1  1 1 1
    --  0 8 6 4  2 0 8 6  4 2 0 8  6 4 2 0
    -- 3 2 2 2  2 2 1 1  1 1 1
    -- 1 9 7 5  3 1 9 7  5 3 1 9  7 5 3 1

    -- 001ddddd 00000000 IIIIIIII IIIIIIII: reg[d] = zext(imm)
    -- 001ddddd 00000001 IIIIIIII IIIIIIII: reg[d] = sext(imm)
    -- 001ddddd 00000010 IIIIIIII IIIIIIII: reg[d] = imm << 16
    -- 001ddddd rrrrr100 IIIIIIII IIIIIIII: reg[d] = mem[reg[r] + sext(imm)]
    -- 001ddddd rrrrr101 IIIIIIII IIIIIIII: mem[reg[d] + sext(imm)] = reg[r]

    -- 01000000 dddddrrr rrsssss0 00000000: reg[d] = reg[r] + reg[s]
    -- 01000001 dddddrrr rrsssss0 00000000: reg[d] = reg[r] - reg[s]
    -- 01000010 dddddrrr rrsssss0 00000000: reg[d] = reg[r] * reg[s]
    -- 01000011 dddddrrr rrsssss0 00000000: reg[d] = reg[r] / reg[s]
    -- 01000100 dddddrrr rrsssss0 00000000: reg[d] = reg[r] & reg[s]
    -- 01000101 dddddrrr rrsssss0 00000000: reg[d] = reg[r] | reg[s]
    -- 01000110 dddddrrr rrsssss0 00000000: reg[d] = reg[r] ^ reg[s]
    -- 01000111 dddddrrr rrsssss0 00000000: reg[d] = ~(reg[r] | reg[s])
    -- 01010000 dddddrrr rrsssss0 00000000: reg[d] = (reg[r] == reg[s]) ? 1 : 0
    -- 01010001 dddddrrr rrsssss0 00000000: reg[d] = (reg[r] < reg[s]) ? 1 : 0
    -- 01010010 dddddrrr rrsssss0 00000000: reg[d] = (reg[r] > reg[s]) ? 1 : 0
    -- 01010101 dddddrrr rrsssss0 00000000: reg[d] = (reg[r] <s reg[s]) ? 1 : 0
    -- 01010110 dddddrrr rrsssss0 00000000: reg[d] = (reg[r] >s reg[s]) ? 1 : 0

    -- 011rrrrr 00000000 IIIIIIII IIIIIIII: if (reg[r] == 0) IP += sext(imm)
    -- 011rrrrr 00000001 IIIIIIII IIIIIIII: if (reg[r] != 0) IP += sext(imm)
    -- 011rrrrr sssss100 00000000 00000000: if (reg[r] == 0) IP = reg[s]
    -- 011rrrrr sssss101 00000000 00000000: if (reg[r] != 0) IP = reg[s]

    -- 10000000 ddddd000 00000000 00000000: reg[d] = IP
    -- 10000001 ddddd000 00000000 00000000: IP = reg[d]
    -- 10000010 00000000 00000000 00000000: freeze
    -- 10000011 00000000 00000000 00000000: nop

    -- 11100000 aaaaa000 00000000 00000000: Dump reg[a]
    -- 11100001 00000000 00000000 00000000: Dump full processor state

    -- register file
    signal WrEn : std_logic;
    signal R1, R2, Wr : reg_select_t;
    signal R1Out, R2Out, WrIn : word_t;

    -- internal registers
    signal IP, IPNext, Instruction : word_t;

    -- ALU regs (C-types)
    signal ALUA, ALUB, ALUOut : word_t;
    signal ALUUnknownOp : std_logic;

    type StateType is (WaitForRun, InsLoadDone, InsExecute, MemLoadDone, Freeze);
    signal current_state, next_state : StateType;

    -- decoded instruction
    signal Class : std_logic_vector(2 downto 0);

    signal BD, BR : reg_select_t;
    signal BO : std_logic_vector(2 downto 0);
    signal BI : std_logic_vector(15 downto 0);

    signal CO : std_logic_vector(4 downto 0);
    signal CD, CR, CS : reg_select_t;

    signal DR, DS : reg_select_t;
    signal DO : std_logic_vector(2 downto 0);
    signal DI : std_logic_vector(15 downto 0);

    signal EO : std_logic_vector(1 downto 0);
    signal ED : reg_select_t;

    signal XO : std_logic_vector(0 downto 0);
    signal XA : reg_select_t;
begin
    Frozen <= '1' when current_state = Freeze else '0';

    regfile: component RegisterFile port map(
        clk => Clock,
        reset => Reset,
        WrEn => WrEn,
        R1 => R1,
        R2 => R2,
        Wr => Wr,
        WrIn => WrIn,
        R1Out => R1Out,
        R2Out => R2Out
    );

    alu_inst: component alu port map(
        A => ALUA,
        B => ALUB,
        C => ALUOut,
        Op => CO,
        UnknownOp => ALUUnknownOp
    );

    -- instruction decoder
    Class <= Instruction(31 downto 29);

    BD <= Instruction(28 downto 24);
    BR <= Instruction(23 downto 19);
    BO <= Instruction(18 downto 16);
    BI <= Instruction(15 downto 0);

    CO <= Instruction(28 downto 24);
    CD <= Instruction(23 downto 19);
    CR <= Instruction(18 downto 14);
    CS <= Instruction(13 downto 9);

    DR <= Instruction(28 downto 24);
    DS <= Instruction(23 downto 19);
    DO <= Instruction(18 downto 16);
    DI <= Instruction(15 downto 0);

    EO <= Instruction(25 downto 24);
    ED <= Instruction(23 downto 19);

    XO <= Instruction(24 downto 24);
    XA <= Instruction(23 downto 19);

    process(Reset, Clock)
    begin
        if Reset = '1' then
            current_state <= WaitForRun;
            IP <= word_0;
        elsif rising_edge(Clock) then
            IP <= IPNext;
            current_state <= next_state;
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
        IPNext <= IP;
        MemoryEnable <= '0';
        WriteEnable <= '0';
        ALUA <= (others => '-');
        ALUB <= (others => '-');

        WrEn <= '0';
        R1 <= (others => '-');
        R2 <= (others => '-');
        Wr <= (others => '-');
        WrIn <= (others => '-');

        case current_state is
            when WaitForRun =>
                if Run = '1' then
                    MemoryEnable <= '1';
                    Address <= IP;
                    next_state <= InsLoadDone;
                else
                    next_state <= WaitForRun;
                end if;
            when InsLoadDone =>
                Instruction <= DIn;
                IPNext <= word_t(unsigned(IP) + 1);
                next_state <= InsExecute;
            when InsExecute =>
                next_state <= Freeze;
                report "insn: " & to_string(Instruction);

                case Class is
                    when "001" => case BO is
                        when "000" =>
                            WrEn <= '1';
                            Wr <= BD; WrIn <= zext_imm(BI);
                            next_state <= WaitForRun;
                        when "001" =>
                            WrEn <= '1';
                            Wr <= BD; WrIn <= sext_imm(BI);
                            next_state <= WaitForRun;
                        when "010" =>
                            WrEn <= '1';
                            Wr <= BD; WrIn <= push_imm(BI);
                            next_state <= WaitForRun;
                        when "100" =>
                            R1 <= BR;
                            MemoryEnable <= '1';
                            Address <= word_t(unsigned(R1Out) + unsigned(sext_imm(BI)));
                            next_state <= MemLoadDone;
                        when "101" =>
                            R1 <= BD;
                            MemoryEnable <= '1';
                            WriteEnable <= '1';
                            Address <= word_t(unsigned(R1Out) + unsigned(sext_imm(BI)));
                            DOut <= R2Out; R2 <= BR;
                            next_state <= WaitForRun;
                        when others =>
                            null;
                    end case;
                    when "010" =>
                        ALUA <= R1Out; R1 <= CR;
                        ALUB <= R2Out; R2 <= CS;

                        if ALUUnknownOp = '0' then
                            WrEn <= '1';
                            Wr <= CD; WrIn <= ALUOut;
                            next_state <= WaitForRun;
                        end if;
                    when "011" => case DO is
                        when "000" =>
                            R1 <= DR;

                            if R1Out = word_0 then
                                IPNext <= word_t(unsigned(IP) + unsigned(sext_imm(DI)));
                            end if;

                            next_state <= WaitForRun;
                        when "001" =>
                            R1 <= DR;

                            if R1Out /= word_0 then
                                IPNext <= word_t(unsigned(IP) + unsigned(sext_imm(DI)));
                            end if;

                            next_state <= WaitForRun;
                        when "100" =>
                            R1 <= DR;
                            R2 <= DS;

                            if R1Out = word_0 then
                                IPNext <= R2Out;
                            end if;

                            next_state <= WaitForRun;
                        when "101" =>
                            R1 <= DR;
                            R2 <= DS;

                            if R1Out /= word_0 then
                                IPNext <= R2Out;
                            end if;

                            next_state <= WaitForRun;
                        when others =>
                            null;
                    end case;
                    when "100" => case EO is
                        when "00" =>
                            WrEn <= '1';
                            Wr <= ED; WrIn <= IP;
                            next_state <= WaitForRun;
                        when "01" =>
                            IPNext <= R1Out; R1 <= ED;
                            next_state <= WaitForRun;
                        when "10" =>
                            next_state <= Freeze;
                        when "11" =>
                            next_state <= WaitForRun;
                        when others =>
                            null;
                    end case;
                    when "111" => case XO is
                        when "0" =>
                            R1 <= XA;
                            report "Register " & integer'image(to_integer(unsigned(XA))) & " has value " & to_string(R1Out);
                            next_state <= WaitForRun;
                        when "1" =>
                            report "todo: full dump" severity FAILURE;
                            next_state <= WaitForRun;
                        when others =>
                            null;
                    end case;
                    when others =>
                        null;
                end case;
            when MemLoadDone =>
                WrEn <= '1';
                Wr <= BD; WrIn <= DIn;
                next_state <= WaitForRun;
            when Freeze =>
                next_state <= Freeze;
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
