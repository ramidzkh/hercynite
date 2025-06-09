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
    signal IP, IPOverrideTo : word_t;
    signal IPOverride : std_logic;

    -- ALU regs (C-types)
    signal ALUA, ALUB, ALUOut : word_t;
    signal ALUOp : std_logic_vector(4 downto 0);

    -- parsed instruction
    type InstructionVariant is (StoreImm, MemLoad, MemStore, AluInst, JEZI, JNZI, JEZR, JNZR, GetIP, SetIP, Freeze, Nop, DumpReg, DumpState);

    function to_string(slv : std_logic_vector) return string is
        variable result : string(slv'range);
    begin
        for i in slv'range loop
            result(i) := character'VALUE(std_ulogic'image(slv(i)));
        end loop;
        return result;
    end;

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

    type ifid_t is record
        enabled : std_logic;
        parsed: ParsedInstruction;
        ip : word_t;
    end record;
    type idex_t is record
        enabled : std_logic;
        parsed: ParsedInstruction;
        rs1v, rs2v : word_t;
        ip : word_t;
        write_back : std_logic;

        branch_taken : std_logic;
        branch_target : word_t;
    end record;
    type exmem_t is record
        enabled : std_logic;
        parsed : ParsedInstruction;
        rdv : word_t;
        write_back : std_logic;
    end record;
    type memwb_t is record
        enabled : std_logic;
        rd : reg_select_t;
        rdv : word_t;
        write_back : std_logic;
    end record;

    signal ifid : ifid_t;
    signal idex : idex_t;
    signal exmem : exmem_t;
    signal memwb : memwb_t;

    signal StartFreeze : std_logic;
begin
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

    -- Frozen state latch
    process(Reset, Clock)
    begin
        if Reset = '1' then
            Frozen <= '0';
        elsif rising_edge(Clock) then
            Frozen <= Frozen or StartFreeze;
        end if;
    end process;

    -- IP counter
    process(Reset, Clock)
    begin
        if Reset = '1' then
            IP <= word_0;
        elsif rising_edge(Clock) then
            if IPOverride = '1' then
                IP <= IPOverrideTo;
            else
                IP <= word_t(unsigned(IP) + 1);
            end if;
        end if;
    end process;

    -- IF stage

    -- Always load instruction
    MemoryEnable <= '1';
    Address <= IP;

    process(all)
    begin
        if Run = '1' then
            ifid <= ('1', parse_instruction(DIn), IP);
        else
            ifid.enabled <= '0';
        end if;
    end process;

    -- ID stage

    -- Load operands
    R1 <= ifid.parsed.rs1;
    R2 <= ifid.parsed.rs2;

    process(all)
    begin
        if ifid.enabled = '1' then
            idex <= (
                enabled => '1',
                parsed => ifid.parsed,
                rs1v => R1Out,
                rs2v => R2Out,
                ip   => word_t(unsigned(ifid.ip)),
                write_back => '0',
                branch_taken => '0',
                branch_target => (others => 'X')
            );

            case idex.parsed.variant is
                when JEZI =>
                    if R1Out = word_0 then
                        idex.branch_taken <= '1';
                        idex.branch_target <= word_t(unsigned(ifid.ip) + unsigned(ifid.parsed.imm));
                    end if;
                when JNZI =>
                    if R1Out /= word_0 then
                        idex.branch_taken <= '1';
                        idex.branch_target <= word_t(unsigned(ifid.ip) + unsigned(ifid.parsed.imm));
                    end if;
                when JEZR =>
                    if idex.rs1v = word_0 then
                        idex.branch_taken <= '1';
                        idex.branch_target <= R2Out;
                    end if;
                when JNZR =>
                    if idex.rs1v /= word_0 then
                        idex.branch_taken <= '1';
                        idex.branch_target <= R2Out;
                    end if;
                when SetIP =>
                    idex.branch_taken <= '1';
                    idex.branch_target <= R1Out;
                when others =>
                    null;
            end case;
        else
            idex.enabled <= '0';
        end if;
    end process;

    -- If a branch was taken, override IP ASAP
    IPOverride <= idex.enabled and idex.branch_taken;
    IPOverrideTo <= idex.branch_target;

    -- EX stage

    -- Prepare ALU with fetched operands
    ALUA <= idex.rs1v;
    ALUB <= idex.rs2v;
    ALUOp <= idex.parsed.aluop;

    process(all)
    begin
        if idex.enabled = '1' then
            exmem <= (
                enabled => '1',
                parsed => idex.parsed,
                rdv  => (others => 'X'),
                write_back => idex.write_back
            );

            case idex.parsed.variant is
                when StoreImm =>
                    exmem.rdv <= idex.parsed.imm;
                    exmem.write_back <= '1';
                when MemLoad =>
                    --MemoryEnable <= '1';
                    --Address <= word_t(unsigned(idex.rs1v) + unsigned(idex.parsed.imm));
                when MemStore =>
                    --MemoryEnable <= '1';
                    --WriteEnable <= '1';
                    --Address <= word_t(unsigned(idex.rs1v) + unsigned(idex.parsed.imm));
                    --DOut <= idex.rs2v;
                when AluInst =>
                    exmem.rdv <= ALUOut;
                    exmem.write_back <= '1';
                when JEZI | JNZI | JEZR | JNZR | SetIP =>
                    null;
                when GetIP =>
                    exmem.rdv <= idex.ip;
                    exmem.write_back <= '1';
                when Freeze =>
                    null;
                when Nop =>
                    null;
                when DumpReg =>
                    report "Register " & integer'image(to_integer(unsigned(idex.parsed.rs1))) & " has value " & to_string(idex.rs1v);
                when DumpState =>
                    report "todo: full dump" severity FAILURE;
            end case;
        else
            exmem.enabled <= '0';
        end if;
    end process;

    -- Freeze as soon as a freeze instruction passes execute
    StartFreeze <= exmem.enabled and '1' when exmem.parsed.variant = Freeze else '0';

    -- MEM stage
    process(all)
    begin
        if exmem.enabled = '1' then
            memwb <= (
                enabled => '1',
                rd => exmem.parsed.rd,
                rdv  => exmem.rdv,
                write_back => exmem.write_back
            );

            if exmem.parsed.variant = MemLoad then
                --memwb.rdv <= DIn;
            end if;
        else
            memwb.enabled <= '0';
        end if;
    end process;

    -- WB stage
    WrEn <= memwb.enabled and memwb.write_back;
    Wr <= memwb.rd;
    WrIn <= memwb.rdv;
end;
