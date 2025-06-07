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
