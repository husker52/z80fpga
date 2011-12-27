-- ////////////////////////////////////////////////////////////////////////////////
-- // Author: lsilvest
-- //
-- // Create Date:   02/03/2008
-- //
-- // Module Name:    sdram
-- //
-- // Target Devices: Altera DE2
-- //
-- // Tool versions:  Quartus II 7.2 Web Edition
-- //
-- //
-- // Description: This module is the top level module for the SDRAM controller.
-- //              It instantiates the PLL, testbench and controller.
-- //
-- ////////////////////////////////////////////////////////////////////////////////
-- // Copyright (c) 2008 Authors
-- //
-- // Permission is hereby granted, free of charge, to any person obtaining a copy
-- // of this software and associated documentation files (the "Software"), to deal
-- // in the Software without restriction, including without limitation the rights
-- // to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- // copies of the Software, and to permit persons to whom the Software is
-- // furnished to do so, subject to the following conditions:
-- //
-- // The above copyright notice and this permission notice shall be included in
-- // all copies or substantial portions of the Software.
-- //
-- // THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- // IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- // FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- // AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- // LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- // OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- // THE SOFTWARE.
-- ////////////////////////////////////////////////////////////////////////////////
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_arith.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;


entity sdram is
port
  (
	CLOCK_50		: in 	std_logic;
	SW				: in	std_logic;
	LEDG			: out	std_logic_vector(1 downto 0);
	LEDR			: out	std_logic_vector(1 downto 0);
   --// SDRAM signals
	DRAM_ADDR		: out	std_logic_vector(11 downto 0);
	DRAM_BA_0		: out	std_logic;
	DRAM_BA_1		: out	std_logic;
	DRAM_CAS_N		: out	std_logic;
	DRAM_CKE		: out	std_logic;
	DRAM_CLK		: out	std_logic;
	DRAM_CS_N		: out	std_logic;
	DRAM_DQ			: inout	std_logic_vector(15 downto 0);
	DRAM_LDQM		: out	std_logic;
	DRAM_UDQM		: out	std_logic;
	DRAM_RAS_N		: out	std_logic;
	DRAM_WE_N		: out	std_logic
   );
end;

architecture tb of sdram is
	signal clk:	std_logic	:= '0';
  signal clk0: 	std_logic; --// 133.333 MHZ
  signal clk1:	std_logic; --// 50 MHZ user side 
  signal clk2:	std_logic; --// 133.333 MHZ -3ns
  signal dram_bank:	std_logic_vector(1 downto 0);
  signal dll_locked:	std_logic;

  signal addr_i:	std_logic_vector(21 downto 0);
  signal dat_i:		std_logic_vector(31 downto 0);
  signal dat_o:		std_logic_vector(31 downto 0);
  signal we_i:		std_logic;
  signal ack_o:		std_logic;
  signal stb_i:		std_logic;
  signal cyc_i:		std_logic;
  signal rst_i:		std_logic;
  signal red_led:	std_logic;
  signal green_led:	std_logic;

  signal counter:	std_logic_vector(31 downto 0);

  component sdpll
	PORT
	(
		inclk0		: IN STD_LOGIC  := '0';
		c0		: OUT STD_LOGIC ;
		c1		: OUT STD_LOGIC ;
		c2		: OUT STD_LOGIC ;
		locked		: OUT STD_LOGIC 
	);
end component;

component sdram_controller
port
  (
	clk_i				: in std_logic;
	dram_clk_i			: in std_logic;
	rst_i				: in std_logic;
	dll_locked			: in std_logic;
-- all ddr signals
	dram_addr			: out	std_logic_vector(11 downto 0);
	dram_bank			: out	std_logic_vector(1 downto 0);
	dram_cas_n			: out	std_logic;
	dram_cke			: out	std_logic;
	dram_clk			: out	std_logic;
	dram_cs_n			: out	std_logic;
	dram_dq				: inout	std_logic_vector(15 downto 0);
	dram_ldqm			: out	std_logic;
	dram_udqm			: out	std_logic;
	dram_ras_n			: out	std_logic;
	dram_we_n			: out	std_logic;
-- wishbone bus
	addr_i				: in	std_logic_vector(21 downto 0);
	dat_i				: in	std_logic_vector(31 downto 0);
	dat_o				: out	std_logic_vector(31 downto 0);
	we_i				: in	std_logic;
	ack_o				: out	std_logic;
	stb_i				: in	std_logic;
	cyc_i				: in	std_logic
   );
end component;

component sdram_rw
port
  (
	clk_i		: in	std_logic;
	rst_i		: in	std_logic;
	addr_i		: out	std_logic_vector(21 downto 0);
	dat_i		: out	std_logic_vector(31 downto 0);
	dat_o		: in	std_logic_vector(31 downto 0);
	we_i		: out	std_logic;
	ack_o		: in	std_logic;
	stb_i		: out	std_logic;
	cyc_i		: out	std_logic;
	green_led	: out std_logic;
	red_led		: out std_logic
   );
end component;

begin
  LEDG(0) <= dll_locked;
  DRAM_BA_1 <= dram_bank(1);
  DRAM_BA_0 <= dram_bank(0);
  rst_i <= '0';
	LEDG(0) <= green_led;
	LEDR(0) <= red_led;

	clk <= not clk after 20ns;
	
   pll_inst : sdpll
  port map  (
     --areset => SW,
     inclk0 => clk,
     c0 => clk0,
     c1 => clk1,
     c2 => clk2,
     locked => dll_locked
     );


   sdram_controller_inst : sdram_controller
  port map
    (
     clk_i => clk0,
     dram_clk_i => clk2,
     rst_i => rst_i,
     dll_locked => dll_locked,
     --// all sdram signals
     dram_addr => DRAM_ADDR,
     dram_bank => dram_bank,
     dram_cas_n => DRAM_CAS_N,
     dram_cke => DRAM_CKE,
     dram_clk => DRAM_CLK,
     dram_cs_n => DRAM_CS_N,
     dram_dq => DRAM_DQ,
     dram_ldqm => DRAM_LDQM,
     dram_udqm => DRAM_UDQM,
     dram_ras_n => DRAM_RAS_N,
     dram_we_n => DRAM_WE_N,
     --// wishbone bus
     addr_i => addr_i,
     dat_i => dat_i,
     dat_o => dat_o,
     we_i => we_i,
     ack_o => ack_o,
     stb_i => stb_i,
     cyc_i => cyc_i
     );


   rw_inst : sdram_rw
   port map
    (
     clk_i => clk1,
     rst_i => rst_i,
     addr_i => addr_i,
     dat_i => dat_i,
     dat_o => dat_o,
     we_i => we_i,
     ack_o => ack_o,
     stb_i => stb_i,
     cyc_i => cyc_i,
     red_led => red_led,
     green_led => green_led
     );


end tb;
