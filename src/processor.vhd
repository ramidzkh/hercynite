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
    signal IP, IPNext : word_t;

    -- ALU regs (C-types)
    signal ALUA, ALUB, ALUOut : word_t;
    signal ALUOp : std_logic_vector(4 downto 0);

    type StateType is (Fetch, Decode, Execute, Memory, Writeback, Freeze);
    signal current_state, next_state : StateType;

    -- parsed instruction
    type InstructionVariant is (StoreImm, MemLoad, MemStore, AluInst, JEZI, JNZI, JEZR, JNZR, GetIP, SetIP, Freeze, Nop, DumpReg, DumpState);

    type ParsedInstruction is record
        variant : InstructionVariant;
        rs1, rs2, rd : reg_select_t;
        imm : word_t;
        aluop : std_logic_vector(4 downto 0);
    end record;

    pure function parse_instruction(Instruction : word_t) return ParsedInstruction is
        variable variant : InstructionVariant := Freeze;
        variable rs1, rs2, rd : reg_select_t;
        variable imm : word_t;
        variable aluop : std_logic_vector(4 downto 0);

        variable Class : std_logic_vector(2 downto 0);

        variable BD, BR : reg_select_t;
        variable BO : std_logic_vector(2 downto 0);
        variable BI : std_logic_vector(15 downto 0);

        variable CO : std_logic_vector(4 downto 0);
        variable CD, CR, CS : reg_select_t;

        variable DR, DS : reg_select_t;
        variable DO : std_logic_vector(2 downto 0);
        variable DI : std_logic_vector(15 downto 0);

        variable EO : std_logic_vector(1 downto 0);
        variable ED : reg_select_t;

        variable XO : std_logic_vector(0 downto 0);
        variable XA : reg_select_t;
    begin
        Class := Instruction(31 downto 29);

        BD := Instruction(28 downto 24);
        BR := Instruction(23 downto 19);
        BO := Instruction(18 downto 16);
        BI := Instruction(15 downto 0);

        CO := Instruction(28 downto 24);
        CD := Instruction(23 downto 19);
        CR := Instruction(18 downto 14);
        CS := Instruction(13 downto 9);

        DR := Instruction(28 downto 24);
        DS := Instruction(23 downto 19);
        DO := Instruction(18 downto 16);
        DI := Instruction(15 downto 0);

        EO := Instruction(25 downto 24);
        ED := Instruction(23 downto 19);

        XO := Instruction(24 downto 24);
        XA := Instruction(23 downto 19);

        case Class is
            when "001" => case BO is
                when "000" =>
                    variant := StoreImm;
                    rd := BD;
                    imm := zext_imm(BI);
                when "001" =>
                    variant := StoreImm;
                    rd := BD;
                    imm := sext_imm(BI);
                when "010" =>
                    variant := StoreImm;
                    rd := BD;
                    imm := push_imm(BI);
                when "100" =>
                    variant := MemLoad;
                    rs1 := BR;
                    rd := BD;
                    imm := sext_imm(BI);
                when "101" =>
                    variant := MemStore;
                    rs1 := BD;
                    rs2 := BR;
                    imm := sext_imm(BI);
                when others => null;
            end case;
            when "010" =>
                variant := AluInst;
                aluop := CO;
                rs1 := CR;
                rs2 := CS;
                rd := CD;
            when "011" => case DO is
                when "000" =>
                    variant := JEZI;
                    rs1 := DR;
                    imm := sext_imm(DI);
                when "001" =>
                    variant := JNZI;
                    rs1 := DR;
                    imm := sext_imm(DI);
                when "100" =>
                    variant := JEZR;
                    rs1 := DR;
                    rs2 := DS;
                when "101" =>
                    variant := JNZR;
                    rs1 := DR;
                    rs2 := DS;
                when others => null;
            end case;
            when "100" => case EO is
                when "00" =>
                    variant := GetIP;
                    rd := ED;
                when "01" =>
                    variant := SetIP;
                    rs1 := ED;
                when "10" =>
                    variant := Freeze;
                when "11" =>
                    variant := Nop;
                when others => null;
            end case;
            when "111" => case XO is
                when "0" =>
                    variant := DumpReg;
                    rs1 := XA;
                when "1" =>
                    variant := DumpState;
                when others => null;
            end case;
            when others => null;
        end case;

        return (variant, rs1, rs2, rd, imm, aluop);
    end;

    type DecoratedInstruction is record
        rs1v, rs2v, rdv : word_t;
        ip : word_t;
        write_back : std_logic;
    end record;

    signal parsed : ParsedInstruction;
    signal decorated : DecoratedInstruction;
    signal exout_ip : word_t;
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
        Op => ALUOp,
        UnknownOp => open
    );

    process(Reset, Clock)
    begin
        if Reset = '1' then
            current_state <= Fetch;
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
        ALUOp <= (others => '-');

        R1 <= (others => '-');
        R2 <= (others => '-');
        Wr <= (others => '-');
        WrEn <= '0';
        WrIn <= (others => '-');

        case current_state is
            when Fetch =>
                MemoryEnable <= '1';
                Address <= IP;

                if Run = '1' then
                    next_state <= Decode;
                else
                    next_state <= Fetch;
                end if;
            when Decode =>
                -- todo: register for parsed/decorated instead of implied memory
                parsed <= parse_instruction(DIn);
                R1 <= parsed.rs1;
                R2 <= parsed.rs2;
                decorated <= (R1Out, R2Out, (others => 'X'), word_t(unsigned(IP) + 1), '0');
                next_state <= Execute;
            when Execute =>
                -- todo: same implied memory issue here
                exout_ip <= decorated.ip;

                case parsed.variant is
                    when StoreImm =>
                        decorated.rdv <= parsed.imm;
                        decorated.write_back <= '1';
                        next_state <= Memory;
                    when MemLoad =>
                        MemoryEnable <= '1';
                        Address <= word_t(unsigned(decorated.rs1v) + unsigned(parsed.imm));
                        next_state <= Memory;
                    when MemStore =>
                        MemoryEnable <= '1';
                        WriteEnable <= '1';
                        Address <= word_t(unsigned(decorated.rs1v) + unsigned(parsed.imm));
                        DOut <= decorated.rs2v;
                        next_state <= Memory;
                    when AluInst =>
                        ALUA <= decorated.rs1v;
                        ALUB <= decorated.rs2v;
                        ALUOp <= parsed.aluop;
                        decorated.rdv <= ALUOut;
                        decorated.write_back <= '1';
                        next_state <= Memory;
                    when JEZI =>
                        if decorated.rs1v = word_0 then
                            exout_ip <= word_t(unsigned(decorated.ip) + unsigned(parsed.imm));
                        end if;

                        next_state <= Memory;
                    when JNZI =>
                        if decorated.rs1v /= word_0 then
                            exout_ip <= word_t(unsigned(decorated.ip) + unsigned(parsed.imm));
                        end if;

                        next_state <= Memory;
                    when JEZR =>
                        if decorated.rs1v = word_0 then
                            exout_ip <= decorated.rs2v;
                        end if;

                        next_state <= Memory;
                    when JNZR =>
                        if decorated.rs1v /= word_0 then
                            exout_ip <= decorated.rs2v;
                        end if;

                        next_state <= Memory;
                    when GetIP =>
                        decorated.rdv <= decorated.ip;
                        decorated.write_back <= '1';
                        next_state <= Memory;
                    when SetIP =>
                        exout_ip <= decorated.rs1v;
                        next_state <= Memory;
                    when Freeze =>
                        next_state <= Freeze;
                    when Nop =>
                        next_state <= Memory;
                    when DumpReg =>
                        report "Register " & integer'image(to_integer(unsigned(parsed.rs1))) & " has value " & to_string(decorated.rs1v);
                        next_state <= Memory;
                    when DumpState =>
                        report "todo: full dump" severity FAILURE;
                        next_state <= Memory;
                end case;
            when Memory =>
                if parsed.variant = MemLoad then
                    decorated.rdv <= DIn;
                end if;

                next_state <= Writeback;
            when Writeback =>
                if decorated.write_back = '1' then
                    WrEn <= '1';
                    Wr <= parsed.rd;
                    WrIn <= decorated.rdv;
                end if;

                IPNext <= exout_ip;
                next_state <= Fetch;
            when Freeze =>
                next_state <= Freeze;
        end case;
    end process;
end;
