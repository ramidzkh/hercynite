library IEEE;
use IEEE.std_logic_1164.all;

package pack is
    subtype word_t is std_logic_vector(31 downto 0);
    subtype imm16 is std_logic_vector(15 downto 0);

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

    -- general purpose registers
    type registers_array is array(0 to 31) of word_t;
    signal R, RNext : registers_array;

    -- internal registers
    signal IP, IPNext, Instruction : word_t;

    -- ALU regs (C-types)
    signal ALUA, ALUB, ALUOut : word_t;
    signal ALUUnknownOp : std_logic;

    type StateType is (WaitForRun, InsLoadSetup, InsLoadHold, InsLoadDone, Blehehehe, MemLoadHold, MemLoadDone, MemStoreHold, Freeze);
    signal current_state, next_state : StateType;

    -- decoded instruction
    signal Class : std_logic_vector(2 downto 0);

    signal BD, BR : std_logic_vector(4 downto 0);
    signal BO : std_logic_vector(2 downto 0);
    signal BI : std_logic_vector(15 downto 0);

    signal CO : std_logic_vector(4 downto 0);
    signal CD, CR, CS : std_logic_vector(4 downto 0);

    signal DR, DS : std_logic_vector(4 downto 0);
    signal DO : std_logic_vector(2 downto 0);
    signal DI : std_logic_vector(15 downto 0);

    signal EO : std_logic_vector(1 downto 0);
    signal ED : std_logic_vector(4 downto 0);

    signal XO : std_logic_vector(0 downto 0);
    signal XA : std_logic_vector(4 downto 0);
begin
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
            R <= (others => (others => '0'));
            IP <= (others => '0');
        elsif rising_edge(Clock) then
            R <= RNext;
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
        RNext <= R;
        IPNext <= IP;
        MemoryEnable <= '0';
        WriteEnable <= '0';
        ALUA <= (others => '-');
        ALUB <= (others => '-');

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
                next_state <= Freeze;
                report "insn: " & to_string(Instruction);

                case Class is
                    when "001" => case BO is
                        when "000" =>
                            RNext(to_integer(unsigned(BD))) <= zext_imm(BI);
                            next_state <= WaitForRun;
                        when "001" =>
                            RNext(to_integer(unsigned(BD))) <= sext_imm(BI);
                            next_state <= WaitForRun;
                        when "010" =>
                            RNext(to_integer(unsigned(BD))) <= push_imm(BI);
                            next_state <= WaitForRun;
                        when "100" =>
                            MemoryEnable <= '1';
                            Address <= word_t(unsigned(R(to_integer(unsigned(BR)))) + unsigned(sext_imm(BI)));
                            next_state <= MemLoadHold;
                        when "101" =>
                            MemoryEnable <= '1';
                            WriteEnable <= '1';
                            Address <= word_t(unsigned(R(to_integer(unsigned(BR)))) + unsigned(sext_imm(BI)));
                            DOut <= R(to_integer(unsigned(DR)));
                            next_state <= MemStoreHold;
                        when others =>
                            null;
                    end case;
                    when "010" =>
                        ALUA <= R(to_integer(unsigned(CR)));
                        ALUB <= R(to_integer(unsigned(CS)));

                        if ALUUnknownOp = '0' then
                            RNext(to_integer(unsigned(CD))) <= ALUOut;
                            next_state <= WaitForRun;
                        end if;
                    when "011" => case DO is
                        when "000" =>
                            if R(to_integer(unsigned(DR))) = (31 downto 0 => '0') then
                                IPNext <= word_t(unsigned(IP) + unsigned(sext_imm(DI)));
                            end if;

                            next_state <= WaitForRun;
                        when "001" =>
                            if R(to_integer(unsigned(DR))) /= (31 downto 0 => '0') then
                                IPNext <= word_t(unsigned(IP) + unsigned(sext_imm(DI)));
                            end if;

                            next_state <= WaitForRun;
                        when "100" =>
                            if R(to_integer(unsigned(DR))) = (31 downto 0 => '0') then
                                IPNext <= R(to_integer(unsigned(DS)));
                            end if;

                            next_state <= WaitForRun;
                        when "101" =>
                            if R(to_integer(unsigned(DR))) /= (31 downto 0 => '0') then
                                IPNext <= R(to_integer(unsigned(DS)));
                            end if;

                            next_state <= WaitForRun;
                        when others =>
                            null;
                    end case;
                    when "100" => case EO is
                        when "00" =>
                            RNext(to_integer(unsigned(ED))) <= IP;
                            next_state <= WaitForRun;
                        when "01" =>
                            IPNext <= R(to_integer(unsigned(ED)));
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
                            report "Register " & integer'image(to_integer(unsigned(XA))) & " has value " & to_string(R(to_integer(unsigned(XA))));
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
            when MemLoadHold =>
                MemoryEnable <= '1';
                Address <= word_t(unsigned(R(to_integer(unsigned(BR)))) + unsigned(sext_imm(BI)));
                next_state <= MemLoadDone;
            when MemLoadDone =>
                RNext(to_integer(unsigned(BD))) <= DIn;
                next_state <= WaitForRun;
            when MemStoreHold =>
                MemoryEnable <= '1';
                WriteEnable <= '1';
                Address <= word_t(unsigned(R(to_integer(unsigned(BR)))) + unsigned(sext_imm(BI)));
                DOut <= R(to_integer(unsigned(DR)));
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

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.pack.all;

entity alu is
    port(
        A, B : in word_t;
        C : out word_t;
        Op : in std_logic_vector(4 downto 0);
        UnknownOp : out std_logic);
end entity;

architecture Mixed of alu is
begin
    process(all)
    begin
        C <= (others => '-');
        UnknownOp <= '1';

        case Op is
            when "00000" =>
                C <= word_t(unsigned(A) + unsigned(B));
                UnknownOp <= '0';
            when "00001" =>
                C <= word_t(unsigned(A) - unsigned(B));
                UnknownOp <= '0';
            when "00010" =>
                -- todo: multiply
                UnknownOp <= '0';
            when "00011" =>
                -- todo: divide
                UnknownOp <= '0';
            when "00100" =>
                C <= word_t(unsigned(A) and unsigned(B));
                UnknownOp <= '0';
            when "00101" =>
                C <= word_t(unsigned(A) or unsigned(B));
                UnknownOp <= '0';
            when "00110" =>
                C <= word_t(unsigned(A) xor unsigned(B));
                UnknownOp <= '0';
            when "00111" =>
                C <= not (word_t(unsigned(A) or unsigned(B)));
                UnknownOp <= '0';
            when "10000" =>
                if unsigned(A) = unsigned(B) then
                    C <= word_t(to_unsigned(1, 32));
                else
                    C <= word_t(to_unsigned(0, 32));
                end if;

                UnknownOp <= '0';
            when "10001" =>
                if unsigned(A) < unsigned(B) then
                    C <= word_t(to_unsigned(1, 32));
                else
                    C <= word_t(to_unsigned(0, 32));
                end if;

                UnknownOp <= '0';
            when "10010" =>
                if unsigned(A) > unsigned(B) then
                    C <= word_t(to_unsigned(1, 32));
                else
                    C <= word_t(to_unsigned(0, 32));
                end if;

                UnknownOp <= '0';
            when "10101" =>
                if signed(A) < signed(B) then
                    C <= word_t(to_unsigned(1, 32));
                else
                    C <= word_t(to_unsigned(0, 32));
                end if;

                UnknownOp <= '0';
            when "10110" =>
                if signed(A) > signed(B) then
                    C <= word_t(to_unsigned(1, 32));
                else
                    C <= word_t(to_unsigned(0, 32));
                end if;

                UnknownOp <= '0';
            when others =>
                null;
        end case;
    end process;
end architecture;
