library ieee;
  use ieee.std_logic_1164.all;
--  use ieee.std_logic_arith.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

entity bintobcd is
	port(
		clk		: in std_logic;
		rst		: in std_logic;
		clken	: in std_logic;
		start	: in std_logic;
		c_in	: in std_logic;
		c_out	: out std_logic;
		q		: out std_logic_vector(3 downto 0)
	);
end;
	
architecture rtl of bintobcd is
	signal q_int	: unsigned(3 downto 0);
	signal q_next	: unsigned(3 downto 0);
	signal q_out	: unsigned(3 downto 0);
begin
    c_out <= q_next(3);
	q_int <= q_next(2 downto 0) & c_in;
    q_out <= q_next + 3 when q_next >= 5 else
		q_int;
	q <= std_logic_vector(q_out);
	
	process(clk,rst)
	begin
	    if rst = '1' then
	        q_next <= "0000";
	    else
			if rising_edge(clk) and clken = '1' then
				if start = '1' then
					q_next <= "0000";
				else
					q_next <= q_out;
				end if;
			end if;
	    end if;
	    
	                
	end process;
end rtl;