library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_arith.all;
  use ieee.std_logic_unsigned.all;

entity debug_tb is
end debug_tb;

architecture tb of debug_tb is


signal CLK : std_logic;
signal OCLK : std_logic;
signal reset_n : std_logic;

signal mon_rxd		: std_ulogic;
signal mon_cts		: std_ulogic;
signal mon_dsr		: std_ulogic;
signal mon_ri		: std_ulogic;
signal mon_dcd		: std_ulogic;

signal mon_txd		: std_ulogic;
signal mon_rts		: std_ulogic;
signal mon_dtr		: std_ulogic;

component DebugSystem is
	port(
		Reset_n		: in std_logic;
		Clk			: in std_logic;
		NMI_n		: in std_logic;
		RXD0		: in std_logic;
		CTS0		: in std_logic;
		DSR0		: in std_logic;
		RI0			: in std_logic;
		DCD0		: in std_logic;
		RXD1		: in std_logic;
		CTS1		: in std_logic;
		DSR1		: in std_logic;
		RI1			: in std_logic;
		DCD1		: in std_logic;
		TXD0		: out std_logic;
		RTS0		: out std_logic;
		DTR0		: out std_logic;
		TXD1		: out std_logic;
		RTS1		: out std_logic;
		DTR1		: out std_logic
	);
end component;


begin
	u0: DebugSystem
	port map(
		Reset_n => reset_n,
		Clk	=> clk,
		NMI_n => '1',

		RXD0 => mon_rxd,
		CTS0 => mon_cts,
		DSR0 => mon_dsr,
		RI0 => mon_ri,
		DCD0 => mon_dcd,

		RXD1 => '1',
		CTS1 => '1',
		DSR1 => '1',
		RI1 => '1',
		DCD1 => '1',

		TXD0 => mon_txd,
		RTS0 => mon_rts,
		DTR0 => mon_dtr
	
	);
	process
	begin
		reset_n <= '1';
		wait for 50 ns;
		reset_n <= '0';
		wait for 50 ns;
		reset_n <= '1';
		wait;
	end process;
	
	process
	begin
		clk <= '0';
		wait for 50 ns;
		clk <= '1';
		wait for 50 ns;
	end process;
end tb;