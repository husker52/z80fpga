-- ////////////////////////////////////////////////////////////////////////////////
-- // Author: lsilvest
-- //
-- // Create Date:   02/03/2008
-- //
-- // Module Name:   sdram_controller
-- //
-- // Target Devices: Altera DE2
-- //
-- // Tool versions:  Quartus II 7.2 Web Edition
-- //
-- //
-- // Description: This module is an SDRAM controller for 8-Mbyte SDRAM chip
-- //              PSC A2V64S40CTP-G7. Corresponding datasheet part number is
-- //              IS42S16400.
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

entity sdram_controller is
generic
(
-- // row width 12
-- // column width 8
-- // bank width 2
-- // user address is specified as {bank,row,column}
-- //                               CAS=3 BL=2
-- //                                 ___ ___
	MODE_REGISTER		: std_logic_vector(11 downto 0) := "000000110001";
	
  
--  // @ 133.333 MHz period is 7.5 nano cycle
  
	TRC_CNTR_VALUE		: integer := 9;           --// 9 cycles, == time to wait after refresh, 67.5ns 
                                                      --// also time to wait between two ACT commands
	RFSH_INT_CNTR_VALUE	: integer := 2000;       --// need 4096 refreshes for every 64_000_000 ns
                                                      --// so the # of cycles between refreshes is
                                                      --// 64000000 / 4096 / 7.5 = 2083
	TRCD_CNTR_VALUE		: integer := 3;           --// ras to cas delay 20ns
                                                      --// will also be used for tRP and tRSC
	WAIT_200us_CNTR_VALUE	: integer := 27000      --// 27000 200us

);
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
end;

architecture rtl of sdram_controller is
	type init_states is (INIT_IDLE,INIT_WAIT_200us,INIT_INIT_PRE,INIT_WAIT_PRE,INIT_MODE_REG,INIT_WAIT_MODE_REG,INIT_DONE_ST);

	type run_states is (IDLE_ST,REFRESH_ST,REFRESH_WAIT_ST,ACT_ST,WAIT_ACT_ST,WRITE0_ST,WRITE1_ST,WRITE_PRE_ST,READ0_ST,READ1_ST,READ2_ST,READ3_ST,READ4_ST,READ_PRE_ST,PRE_ST,WAIT_PRE_ST);

	signal address_r		: std_logic_vector(21 downto 0);  
	
	signal dram_addr_r		: std_logic_vector(11 downto 0);
	signal dram_bank_r		: std_logic_vector(1 downto 0);
	signal dram_dq_r		: std_logic_vector(15 downto 0);  
	signal dram_cas_n_r	: std_logic;
	signal dram_ras_n_r	: std_logic;
	signal dram_we_n_r		: std_logic;
	
	
	signal dat_o_r			: std_logic_vector(31 downto 0)	:= X"00000000";
	signal ack_o_r			: std_logic						:= '0';
	signal dat_i_r			: std_logic_vector(31 downto 0);
	signal we_i_r			: std_logic;
	signal stb_i_r			: std_logic;
	signal oe_r			: std_logic							:= '0';
	
	signal current_state		: run_states;
	signal next_state			: run_states;
	signal current_init_state	: init_states;
	signal next_init_state		: init_states;
	
	
	signal init_done			: std_logic	:= '1';
	signal init_pre_cntr		: integer	:= 0;
	signal trc_cntr			: integer	:= 0;
	signal rfsh_int_cntr		: integer	:= 0;      
	signal trcd_cntr			: integer	:= 0;
	signal wait_200us_cntr		: integer	:= 0;
	signal do_refresh			: integer;
  
 begin 
	dram_addr <= dram_addr_r;
	dram_bank <= dram_bank_r;
	dram_cas_n <= dram_cas_n_r;
	dram_ras_n <= dram_ras_n_r;
	dram_we_n <= dram_we_n_r;
	dram_dq <= dram_dq_r when oe_r = '1' else "ZZZZZZZZZZZZZZZZ";
	
	dat_o <= dat_o_r;
	ack_o <= ack_o_r;
	
	dram_cke <= '1'; --// dll_locked
	dram_cs_n <=  not dll_locked;  --// chip select is always on in normal op
	dram_clk <= dram_clk_i;
	dram_ldqm <= '0';         --// don't do byte masking
	dram_udqm <= '0';         --// don't do byte masking
  


  --// register the user command
  process(clk_i)
  begin
	if(rising_edge(clk_i)) then
		if (stb_i_r = '1' and current_state = ACT_ST) then
		  stb_i_r <= '0';
		elsif (stb_i = '1' and cyc_i = '1') then
		  address_r <= addr_i;
		  dat_i_r <= dat_i;
		  we_i_r <= we_i;
		  stb_i_r <= stb_i;
		end if;
	end if;
  end process;
  
process(clk_i)
begin
	if(rising_edge(clk_i)) then
		if (rst_i = '1') then
		  wait_200us_cntr <= 0;
		elsif (current_init_state = INIT_IDLE) then
		  wait_200us_cntr <= WAIT_200us_CNTR_VALUE;
		else
		  wait_200us_cntr <= wait_200us_cntr - 1;
		end if;
	end if;
end process;


  --// control the interval between refreshes:
process(clk_i)
begin
	if(rising_edge(clk_i)) then
		if (rst_i = '1') then
		  rfsh_int_cntr <= 0;   --// immediately initiate new refresh on reset
		elsif (current_state = REFRESH_WAIT_ST) then
		  do_refresh <= 0;
		  rfsh_int_cntr <= RFSH_INT_CNTR_VALUE;
		elsif (rfsh_int_cntr = 0) then
		  do_refresh <= 1;
		else
		  rfsh_int_cntr <= rfsh_int_cntr - 1; 
		end if;
	end if;
end process;
  
process(clk_i)
begin
	if(rising_edge(clk_i)) then
		if (rst_i = '1') then
		  trc_cntr <= 0;
		elsif (current_state = PRE_ST or current_state = REFRESH_ST) then
		  trc_cntr <= TRC_CNTR_VALUE;
		else
		  trc_cntr <= trc_cntr - 1;
		end if;
	end if;
end process;


--  // counter to control the activate
process(clk_i)
begin
	if (rising_edge(clk_i)) then
		if (rst_i = '1') then
		  trcd_cntr <= 0;
		elsif (current_state = ACT_ST or
					 current_init_state = INIT_INIT_PRE or
					 current_init_state = INIT_MODE_REG) then
		  trcd_cntr <= TRCD_CNTR_VALUE;
		else
		  trcd_cntr <= trcd_cntr - 1;
		end if;
	end if;
end process;

process(clk_i)
begin
	if(rising_edge(clk_i)) then
		if (rst_i = '1') then
		  init_pre_cntr <= 0;
		elsif (current_init_state = INIT_INIT_PRE) then
		  init_pre_cntr <= init_pre_cntr + 1;
		end if;
	end if;
end process;

process(clk_i)
begin
	if(rising_edge(clk_i)) then
		if (current_init_state = INIT_DONE_ST) then
		  init_done <= '1';
		end if;
	end if;
end process;

--  // state change
process(clk_i)
begin
	if(rising_edge(clk_i)) then
		if (rst_i = '1') then
		  current_init_state <= INIT_IDLE;
		else
		  current_init_state <= next_init_state;
		end if;
	end if;
end process;

process(clk_i)
begin
	if(rising_edge(clk_i)) then
		if (rst_i = '1') then
		  current_state <= IDLE_ST;
		else
		  current_state <= next_state;
		end if;
	end if;
end process;
  

--  // initialization is fairly easy on this chip: wait 200us then issue
--  // 8 precharges before setting the mode register
process(current_init_state)
begin
    case current_init_state is
      when INIT_IDLE =>
        if (init_done = '0') then
			next_init_state <= INIT_WAIT_200us;
        else                              
			next_init_state <= INIT_IDLE;
		end if;
      when INIT_WAIT_200us =>
        if (wait_200us_cntr /= 0) then            
			next_init_state <= INIT_INIT_PRE;
        else                              
			next_init_state <= INIT_WAIT_200us;
		end if;
      when INIT_INIT_PRE =>                 
		next_init_state <= INIT_WAIT_PRE;

      when INIT_WAIT_PRE =>
        if (trcd_cntr /= 0) then                  --// this is tRP
			if (init_pre_cntr = 8) then      
				next_init_state <= INIT_MODE_REG;
			else                            
				next_init_state <= INIT_INIT_PRE;
			end if;
        else                              
			next_init_state <= INIT_WAIT_PRE;
		end if;

      when INIT_MODE_REG =>                      
		next_init_state <= INIT_WAIT_MODE_REG;
      
      when INIT_WAIT_MODE_REG =>
        if (trcd_cntr /= 0) then --/* tRSC */        
			next_init_state <= INIT_DONE_ST;
        else                              
			next_init_state <= INIT_WAIT_MODE_REG;
		end if;
      when INIT_DONE_ST =>                       
		next_init_state <= INIT_IDLE;

      when others =>                            
		next_init_state <= INIT_IDLE;
      
    end case;
  end process;


--  // this is the main controller logic:
process(current_state)
begin
    case current_state is
      when IDLE_ST =>
        if (init_done = '0') then               
			next_state <= IDLE_ST;
        elsif (do_refresh = 1) then       
			next_state <= REFRESH_ST;
        elsif (stb_i_r = '1') then       
			next_state <= ACT_ST;
        else                          
			next_state <= IDLE_ST;
		end if;
      when REFRESH_ST =>                     
		next_state <= REFRESH_WAIT_ST;

      when REFRESH_WAIT_ST =>
        if (trc_cntr /= 0) then
			next_state <= IDLE_ST;
        else                          
			next_state <= REFRESH_WAIT_ST;
		end if;
      when ACT_ST =>                         
		next_state <= WAIT_ACT_ST;
      
      when WAIT_ACT_ST =>
		if (trcd_cntr /= 0) then 
			if (we_i_r = '1') then                 
				next_state <= WRITE0_ST;
			else                        
				next_state <= READ0_ST;
			end if;
		else                           
			next_state <= WAIT_ACT_ST;
		end if;
      when WRITE0_ST =>                      
		next_state <= WRITE1_ST;

      when WRITE1_ST =>                      
		next_state <= WRITE_PRE_ST;
      
      when WRITE_PRE_ST =>                   
		next_state <= PRE_ST;
      
      when READ0_ST =>                       
		next_state <= READ1_ST;

      when READ1_ST =>                       
		next_state <= READ2_ST;
      
      when READ2_ST =>                       
		next_state <= READ3_ST;

      when READ3_ST =>                       
		next_state <= READ4_ST;

      when READ4_ST =>                       
		next_state <= READ_PRE_ST;

      when READ_PRE_ST =>                    
		next_state <= PRE_ST;
      
      when PRE_ST =>                         
		next_state <= WAIT_PRE_ST;
      
      when WAIT_PRE_ST =>
        --// if the next command was not another row activate in the same bank
        --// we could wait tRCD only; for simplicity but at the detriment of
        --// efficiency we always wait tRC
        if (trc_cntr /= 0) then
			next_state <= IDLE_ST;
        else                          
			next_state <= WAIT_PRE_ST;
		end if;
      when others =>                       
		next_state <= IDLE_ST;        

    end case;
  end process;

  
  --// ack_o signal
process(clk_i)
begin
	if (rising_edge(clk_i)) then
    if (current_state = READ_PRE_ST or
        current_state = WRITE_PRE_ST) then
      ack_o_r <= '1';
    elsif (current_state = WAIT_PRE_ST) then
      ack_o_r <= '0';
    end if;
	end if;
  end process;

  
--  // data
process(clk_i)
begin
	if(rising_edge(clk_i)) then
    if (rst_i = '1') then
      dat_o_r <= X"00000000";
      dram_dq_r <= X"0000";
      oe_r <= '0';
    elsif (current_state = WRITE0_ST) then
      dram_dq_r <= dat_i_r(31 downto 16);
      oe_r <= '1';
    elsif (current_state = WRITE1_ST) then
      dram_dq_r <= dat_i_r(15 downto 0);
      oe_r <= '1';
    elsif (current_state = READ4_ST) then
      --// we should actually be reading this on READ3, but
      --// because of delay the data comes a cycle later...
      dat_o_r(31 downto 16) <= dram_dq;
      dram_dq_r <= "ZZZZZZZZZZZZZZZZ";
      oe_r <= '0';
    elsif (current_state = READ_PRE_ST) then
      dat_o_r(15 downto 0) <= dram_dq;
      dram_dq_r <= "ZZZZZZZZZZZZZZZZ";
      oe_r <= '0';
    else
      dram_dq_r <= "ZZZZZZZZZZZZZZZZ";
      oe_r <= '0';
    end if;
	end if;
  end process;

  
--  // address
process(clk_i)
begin
	if(rising_edge(clk_i)) then
    if (current_init_state = INIT_MODE_REG) then
      dram_addr_r <= MODE_REGISTER;
    elsif (current_init_state = INIT_INIT_PRE) then
      dram_addr_r <= "10000000000";  --// precharge all
    elsif (current_state = ACT_ST) then
      dram_addr_r <= address_r(19 downto 8);
      dram_bank_r <= address_r(21 downto 20);
    elsif (current_state = WRITE0_ST or current_state = READ0_ST) then
      --// enter column with bit a10 set to 1 indicating auto precharge:
      dram_addr_r <= ("0100" & address_r(7 downto 0));
      dram_bank_r <= address_r(21 downto 20);
    else
      dram_addr_r <= "000000000000";
      dram_bank_r <= "00";
    end if;
	end if;
  end process;

  
--  // commands
process(clk_i)
begin
	if(rising_edge(clk_i)) then
		if(current_init_state = INIT_INIT_PRE or
						 current_init_state = INIT_MODE_REG or
						 current_state = REFRESH_ST or
						 current_state = ACT_ST) then 
			dram_ras_n_r <= '0';
		else
			dram_ras_n_r <= '1';
		end if;
		if (current_state = READ0_ST or
						 current_state = WRITE0_ST or
						 current_state = REFRESH_ST or
						 current_init_state = INIT_MODE_REG) then
				dram_cas_n_r <= '0';
			 else
				dram_cas_n_r <= '1';
			 end if;
		if (current_init_state = INIT_INIT_PRE or
						current_state = WRITE0_ST or
						current_init_state = INIT_MODE_REG) then
				dram_we_n_r <= '0';
			else 
				dram_we_n_r <= '1';
		end if;
	end if;
end process;

end rtl;
