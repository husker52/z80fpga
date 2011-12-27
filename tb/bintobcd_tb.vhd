library ieee;
  use ieee.std_logic_1164.all;
--  use ieee.std_logic_arith.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

entity bintobcd_sim is
end bintobcd_sim;

architecture tb of bintobcd_sim is

signal CLK : std_logic;
signal reset : std_logic;
signal clken : std_logic;
signal start : std_logic;
signal c_in : std_logic;
signal c_out : std_logic;
signal q : std_logic_vector(3 downto 0);
signal cnt : unsigned(3 downto 0);
signal bcd : unsigned(3 downto 0);

component bintobcd is
	port(
		clk		: in std_logic;
		rst		: in std_logic;
		clken	: in std_logic;
		start	: in std_logic;
		c_in	: in std_logic;
		c_out	: out std_logic;
		q		: out std_logic_vector(3 downto 0)
	);
end component;

begin
    u0 : bintobcd
    port map(
		clk => clk,		
		rst	=> reset,
		clken => clken,
		start => start,
		c_in => c_in,
		c_out => c_out,
		q => q
    );
	
	process
	begin
		reset <= '0';
		wait for 50 ns;
		reset <= '1';
		wait for 200 ns;
		reset <= '0';
		wait;
	end process;
	
	process
	begin
		clk <= '0';
		wait for 50 ns;
		clk <= '1';
		wait for 50 ns;
	end process;
	
	process
--		variable count : unsigned(3 downto 0);
		variable q_in : unsigned(3 downto 0);
	begin
		start <= '0';
		c_in <= '0';
		wait until falling_edge(reset);
		for count in 0 to 15 loop
			bcd <= to_unsigned(count,4);
			q_in := to_unsigned(count,4);
			wait until rising_edge(clk);
			clken <= '1';
			start <= '1';
			wait until rising_edge(clk);
			start <= '0';
			for i in 0 to 3 loop
				cnt <= to_unsigned(i,4);
				c_in <= q_in(3);
				q_in := q_in(2 downto 0) & '0';
				wait until rising_edge(clk);
			end loop;
			clken <= '0';
			wait until rising_edge(clk);
			wait until rising_edge(clk);
		end loop;
	    wait;
	    
	end process;
		
end tb;