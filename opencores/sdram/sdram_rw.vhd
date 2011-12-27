-- ////////////////////////////////////////////////////////////////////////////////
-- // Author: lsilvest
-- //
-- // Create Date:   02/03/2008
-- //
-- // Module Name:    sdram_rw
-- //
-- // Target Devices: Altera DE2
-- //
-- // Tool versions:  Quartus II 7.2 Web Edition
-- //
-- //
-- // Description: This module provides a simple test bench for the SDRAM
-- //              controller.  It sequentially writes all positions in
-- //              memory, pauses for a while and then reads back all
-- //              positions comparing them to the written value. The
-- //              green LEDG1 indicates the test passed. The red LEDR0
-- //              indicates at least one of the readbacks failed
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
  
entity sdram_rw is
generic(
  MAX_RW 	: std_logic_vector(23 downto 0) := std_logic_vector(to_unsigned(2000000,24)); --24'd200000; // 200000 is the full 8 Mbytes of memory
  R_TO_W_WAIT_TIME  : integer := 12500000; --24'd12500000;
  INITIAL_MEM_VALUE : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(12345678,32))	--32'd12345678;

);
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
end;
  
architecture rtl of sdram_rw is  
	type test_states is (START_WRITE_ST,WRITE_ST,WAIT_WRITE_ACK_ST,READ_ST,WAIT_READ_ACK_ST,WRITE_WAIT_ST,START_READ_ST,READ_WAIT_ST,DONE_ST);

	signal addr_i_r		: std_logic_vector(21 downto 0) := std_logic_vector(to_unsigned(0,22));
	signal dat_i_r		: std_logic_vector(31 downto 0);
	signal we_i_r		: std_logic := '0';
	signal stb_i_r		: std_logic := '0';
	signal cyc_i_r		: std_logic := '0';

	signal rw_cntr		: integer := 0;
	signal cntr			: integer := 0;
	signal number		: std_logic_vector(31 downto 0);
	signal mem_value	: std_logic_vector(31 downto 0) := INITIAL_MEM_VALUE;
	signal state		: test_states := START_WRITE_ST;   

	signal red_led_r	: std_logic := '0';
	signal green_led_r 	: std_logic := '0';

	
begin
	dat_i <= dat_i_r;
	addr_i <= addr_i_r;
	we_i <= we_i_r;
	stb_i <= stb_i_r;
	cyc_i <= cyc_i_r;
	red_led <= red_led_r;
	green_led <= green_led_r;


process(clk_i)
begin
	if(rising_edge(clk_i)) then
    if (rst_i = '1') then
      state <= START_WRITE_ST;
      red_led_r <= '0';
    else
      case (state) is
        when START_WRITE_ST =>
            state <= WRITE_ST;
            addr_i_r <= std_logic_vector(to_unsigned(0,22));
            rw_cntr <= 0;
            mem_value <= INITIAL_MEM_VALUE;
        
        when WRITE_ST =>
            stb_i_r <= '1';
            cyc_i_r <= '1';
            dat_i_r <= mem_value;
            we_i_r <= '1';
            state <= WAIT_WRITE_ACK_ST;

        when WAIT_WRITE_ACK_ST =>
			if (ack_o = '1') then
				state <= WRITE_WAIT_ST;
				stb_i_r <= '0';
				cyc_i_r <= '0';
			end if;
        when WRITE_WAIT_ST =>
            mem_value <= mem_value + 1;
            if (rw_cntr < MAX_RW - 1) then
              addr_i_r <= addr_i_r + 2;
              rw_cntr <= rw_cntr + 1;
              state <= WRITE_ST;
            else 
              state <= START_READ_ST;
            end if;

        when START_READ_ST =>
			if (cntr /= 0) then -- // wait for R_TO_W_WAIT_TIME
				state <= READ_ST;
				addr_i_r <= std_logic_vector(to_unsigned(0,22));
				rw_cntr <= 0;
				mem_value <= INITIAL_MEM_VALUE;
			else 
				state <= START_READ_ST;
			end if;
        when READ_ST =>
            stb_i_r <= '1';
            cyc_i_r <= '1';
            we_i_r <= '0';
            state <= WAIT_READ_ACK_ST;
        
        when WAIT_READ_ACK_ST =>
          if (ack_o = '1') then
            state <= READ_WAIT_ST;
            number <= dat_o;
            stb_i_r <= '0';
            cyc_i_r <= '0';
          end if;

        when READ_WAIT_ST =>
            if (rw_cntr < MAX_RW - 1) then
              if (mem_value /= number) then
                red_led_r <= '1';
              end if;
              mem_value <= mem_value + 1;
              rw_cntr <= rw_cntr + 1;
              state <= READ_ST;
              --// increment address:
              addr_i_r <= addr_i_r + 2;
            else 	-- // if (rw_cntr < MAX_RW - 1)
              --//state <= START_WRITE_ST;
              state <= DONE_ST;
            end if;
          --// case: READ_WAIT_ST

        when DONE_ST =>
            state <= DONE_ST;
            if (red_led_r /= '0') then
              green_led_r <= '1';
            end if;
      end case; -- // case (state)
    end if; --// else: !if(rst_i)
end if;
  end process; --// always@ (posedge clk_i)

process(clk_i)
begin
	if(rising_edge(clk_i)) then
		if (rst_i = '1') then
		  cntr <= 0;
		elsif (state = WRITE_WAIT_ST) then
		  cntr <= R_TO_W_WAIT_TIME;
		else
		  cntr <= cntr - 1;
		end if;
	end if;
end process;
  
  
end rtl;

