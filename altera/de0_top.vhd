library ieee;
  use ieee.std_logic_1164.all;
--  use ieee.std_logic_arith.all;
--  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

entity de0_top is
	port
	(
		--////////////////////	Clock Input	 	////////////////////	 
		CLOCK_50		: in std_ulogic;						--	50 MHz
		CLOCK_50_2		: in std_ulogic;						--	50 MHz
		--////////////////////	Push Button		////////////////////
		BUTTON			: in std_ulogic_vector(2 downto 0);							--	Pushbutton[2:0]
		--////////////////////	DPDT Switch		////////////////////
		SW				: in std_logic_vector(9 downto 0);								--	Toggle Switch[9:0]
		--////////////////////	7-SEG Dispaly	////////////////////
		HEX0_D			: out std_logic_vector(6 downto 0);							--	Seven Segment Digit 0
		HEX0_DP			: out std_logic;						--	Seven Segment Digit DP 0
		HEX1_D			: out std_logic_vector(6 downto 0);							--	Seven Segment Digit 1
		HEX1_DP			: out std_logic;						--	Seven Segment Digit DP 1
		HEX2_D			: out std_logic_vector(6 downto 0);							--	Seven Segment Digit 2
		HEX2_DP			: out std_logic;						--	Seven Segment Digit DP 2
		HEX3_D			: out std_logic_vector(6 downto 0);							--	Seven Segment Digit 3
		HEX3_DP			: out std_logic;						--	Seven Segment Digit DP 3
		--////////////////////////	LED		////////////////////////
		LEDG			: out std_ulogic_vector(9 downto 0);							--	LED Green[9:0]
		--////////////////////////	UART	////////////////////////
		UART_TXD		: out std_ulogic;						--	UART Transmitter
		UART_RXD		: in std_ulogic;						--	UART Receiver
		UART_CTS		: out std_ulogic;						--	UART Clear To Send
		UART_RTS		: in std_ulogic;						--	UART Request To Send
		--/////////////////////	SDRAM Interface		////////////////
		DRAM_DQ			: inout std_ulogic_vector(15 downto 0);						--	SDRAM Data bus 16 Bits
		DRAM_ADDR		: out std_ulogic_vector(12 downto 0);						--	SDRAM Address bus 13 Bits
		DRAM_LDQM		: out std_ulogic;						--	SDRAM Low-byte Data Mask 
		DRAM_UDQM		: out std_ulogic;						--	SDRAM High-byte Data Mask
		DRAM_WE_N		: out std_ulogic;						--	SDRAM Write Enable
		DRAM_CAS_N		: out std_ulogic;						--	SDRAM Column Address Strobe
		DRAM_RAS_N		: out std_ulogic;						--	SDRAM Row Address Strobe
		DRAM_CS_N		: out std_ulogic;						--	SDRAM Chip Select
		DRAM_BA_0		: out std_ulogic;						--	SDRAM Bank Address 0
		DRAM_BA_1		: out std_ulogic;						--	SDRAM Bank Address 1
		DRAM_CLK		: out std_ulogic;						--	SDRAM Clock
		DRAM_CKE		: out std_ulogic;						--	SDRAM Clock Enable
		--////////////////////	Flash Interface		////////////////
		FL_DQ			: inout std_ulogic_vector(14 downto 0);							--	FLASH Data bus 15 Bits
		FL_DQ15_AM1		: inout std_ulogic;					--	FLASH Data bus Bit 15 or Address A-1
		FL_ADDR			: out std_ulogic_vector(21 downto 0);						--	FLASH Address bus 22 Bits
		FL_WE_N			: out std_ulogic;						--	FLASH Write Enable
		FL_RST_N		: out std_ulogic;						--	FLASH Reset
		FL_OE_N			: out std_ulogic;						--	FLASH Output Enable
		FL_CE_N			: out std_ulogic;						--	FLASH Chip Enable
		FL_WP_N			: out std_ulogic;						--	FLASH Hardware Write Protect
		FL_BYTE_N		: out std_ulogic; 						--	FLASH Selects 8/16-bit mode
		FL_RY			: in std_ulogic;							--	FLASH Ready/Busy
		--////////////////////	LCD Module 16X2		////////////////
		LCD_BLON		: out std_ulogic;						--	LCD Back Light ON/OFF
		LCD_RW			: out std_ulogic;							--	LCD Read/Write Select, 0 = Write, 1 = Read
		LCD_EN			: out std_ulogic;							--	LCD Enable
		LCD_RS			: out std_ulogic;							--	LCD Command/Data Select, 0 = Command, 1 = Data
		LCD_DATA		: inout std_ulogic_vector(7 downto 0);						--	LCD Data bus 8 bits
		--////////////////////	SD_Card Interface	////////////////
		SD_DAT0			: inout std_ulogic;						--	SD Card Data 0
		SD_DAT3			: inout std_ulogic;						--	SD Card Data 3
		SD_CMD			: inout std_ulogic;							--	SD Card Command Signal
		SD_CLK			: out std_ulogic;							--	SD Card Clock
		SD_WP_N			: in std_ulogic;						--	SD Card Write Protect
		--////////////////////	PS2		////////////////////////////
		PS2_KBDAT		: inout std_ulogic;						--	PS2 Keyboard Data
		PS2_KBCLK		: inout std_ulogic;						--	PS2 Keyboard Clock
		PS2_MSDAT		: inout std_ulogic;						--	PS2 Mouse Data
		PS2_MSCLK		: inout std_ulogic;						--	PS2 Mouse Clock
		--////////////////////	VGA		////////////////////////////
		VGA_HS			: out std_ulogic;							--	VGA H_SYNC
		VGA_VS			: out std_ulogic;							--	VGA V_SYNC
		VGA_R			: out std_logic_vector(3 downto 0);   						--	VGA Red[3:0]
		VGA_G			: out std_logic_vector(3 downto 0);	 						--	VGA Green[3:0]
		VGA_B			: out std_logic_vector(3 downto 0);  						--	VGA Blue[3:0]
		--////////////////////	GPIO	////////////////////////////
		GPIO0_CLKIN		: in std_ulogic_vector(1 downto 0);					--	GPIO Connection 0 Clock In Bus
		GPIO0_CLKOUT	: out std_ulogic_vector(1 downto 0);					--	GPIO Connection 0 Clock Out Bus
		GPIO0_D			: inout std_ulogic_vector(31 downto 0);						--	GPIO Connection 0 Data Bus
		GPIO1_CLKIN		: in std_ulogic_vector(1 downto 0);					--	GPIO Connection 1 Clock In Bus
		GPIO1_CLKOUT	: out std_ulogic_vector(1 downto 0);					--	GPIO Connection 1 Clock Out Bus
		GPIO1_D			: inout std_ulogic_vector(31 downto 0)							--	GPIO Connection 1 Data Bus
		
	);
end de0_top;

-- Library Clause(s) (optional)
-- Use Clause(s) (optional)

architecture rtl of de0_top is

signal divider1	: unsigned(1 downto 0);
signal divider2 : unsigned(2 downto 0);
signal CLK_12	: std_logic;
signal CLK_6	: std_logic;
signal vga_hs_int : std_logic;
signal vga_vs_int : std_logic;

signal mon_rxd		: std_ulogic;
signal mon_cts		: std_ulogic;
signal mon_dsr		: std_ulogic;
signal mon_ri		: std_ulogic;
signal mon_dcd		: std_ulogic;

signal mon_txd		: std_ulogic;
signal mon_rts		: std_ulogic;
signal mon_dtr		: std_ulogic;


component SEG7_LUT_4
	PORT
	(
		oSEG0		: out std_logic_vector(6 downto 0);
		oSEG0_DP	: out std_logic;
		oSEG1		: out std_logic_vector(6 downto 0);
		oSEG1_DP	: out std_logic;
		oSEG2		: out std_logic_vector(6 downto 0);
		oSEG2_DP	: out std_logic;
		oSEG3		: out std_logic_vector(6 downto 0);
		oSEG3_DP	: out std_logic;
		
		iDIG		: in std_logic_vector(15 downto 0)
	);
end component;

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
		DTR1		: out std_logic;
	
		uart0_cs_out : out std_logic

	);
end component;

component BCDConv is
  generic (N   : positive);     -- number of digits
  port (Clock  : in std_logic;
        Reset  : in std_logic;
        Init   : in std_logic;  -- initialise conversion
        ModIn  : in std_logic;  -- carry in from outside
        ModOut : out std_logic; -- carry out 
        Q      : out std_logic_vector(4*N -1 downto 0) -- BCD result
       );
end component;

signal ROM_DATA : std_logic_vector(7 downto 0);
signal ROM_ADDR : std_logic_vector(18 downto 0);
signal WE_L : std_logic;
signal OE_L : std_logic;
signal CE_L : std_logic;
signal disp_cnt : unsigned(15 downto 0);
signal disp_divide : unsigned(25 downto 0);
signal seconds : unsigned(6 downto 0);
signal minutes : unsigned(6 downto 0);
signal disp_count : std_logic;

signal vga_vs_last : std_logic;
signal cnt_edge : std_logic;
signal digits : std_logic_vector(15 downto 0);
signal sec_tmp	: unsigned(7 downto 0);
signal min_tmp	: unsigned(7 downto 0);
signal dcnt		: unsigned(4 downto 0);
signal bcd_st	: std_logic;

begin
	ledg(0) <= button(2);
	ledg(1) <= sw(0);
	ledg(2) <= '1';
	ledg(3) <= mon_rxd;
	
	u0 : DebugSystem
	port map(
		Reset_n => button(2),
		Clk	=> clock_50,
		NMI_n => cnt_edge,

		RXD0 => UART_RXD,
		CTS0 => UART_RTS,
		DSR0 => mon_dsr,
		RI0 => '1',
		DCD0 => '1',

		RXD1 => '1',
		CTS1 => '1',
		DSR1 => '1',
		RI1 => '1',
		DCD1 => '1',

		TXD0 => UART_TXD,
		RTS0 => UART_CTS,
--		DTR0 => mon_dtr,
		
		uart0_cs_out => gpio0_d(8)
--		TXD1		
--		RTS1		
--		DTR1		
	);

	u1 :  BCDConv
		generic map(N => 2)     -- number of digits
		port map(
			Clock	=> clock_50,
			Reset	=> '0',
			Init	=> bcd_st,
			ModIn	=> min_tmp(7),
--			ModOut 
			Q => digits(15 downto 8)
	);
	u2 :  BCDConv
		generic map(N => 2)     -- number of digits
		port map(
			Clock	=> clock_50,
			Reset	=> '0',
			Init	=> bcd_st,
			ModIn	=> sec_tmp(7),
--			ModOut 
			Q => digits(7 downto 0)
	);
	
	gpio0_d(0) <= mon_txd;
	gpio0_d(1) <= mon_rts;
	gpio0_d(2) <= mon_dtr;
	mon_rxd <= gpio0_d(3);
	gpio0_d(4) <= mon_cts;
	gpio0_d(5) <= mon_dsr;
	gpio0_d(6) <= mon_ri;
	gpio0_d(7) <= mon_dcd;
	
	gpio0_d(31) <= mon_txd;
--	gpio0_d(30) <= mon_rxd;
--	gpio0_d(3) <= '1';
	
--	process(clock_50)
--	begin
--		if rising_edge(clock_50) then
--			vga_vs_last <= vga_vs_int;
--			if vga_vs_int /= vga_vs_last then
--				cnt_edge <= '1';
--			else
--				cnt_edge <= '0';
--			end if;
--		end if;
--	end process;

	process(clock_50)
	begin
		if rising_edge(clock_50) then
			if disp_divide < 50000000 then
				disp_divide <= disp_divide + 1;
			else
				disp_divide <= to_unsigned(0,26);
			end if;
		end if;
		if disp_divide = 0 then
			cnt_edge <= '1';
		else
			cnt_edge <= '0';
		end if;
	end process;
	
	process(button(2),clock_50)
	begin
		if button(2) = '0' then
			disp_cnt <= X"0000";
			seconds <= to_unsigned(0,7);
			minutes <= to_unsigned(0,7);
		elsif rising_edge(clock_50) then
			if cnt_edge = '1' then
				disp_cnt <= disp_cnt + 1;
			else
				disp_cnt <= disp_cnt;
			end if;
			if cnt_edge = '1' then
				if seconds = to_unsigned(59,7) then
					seconds <= to_unsigned(0,7);
					if minutes = to_unsigned(59,7) then
						minutes <= to_unsigned(0,7);
					else
						minutes <= minutes + 1;
					end if;
				else
					seconds <= seconds + 1;
				end if;
			end if;
		end if;
	end process;
	
	bcd_st <= cnt_edge;
	
	process(button(2),clock_50)
	begin
		if button(2) = '0' then
			sec_tmp <= X"00";
			min_tmp <= X"00";
			dcnt <= to_unsigned(0,5);
		elsif rising_edge(clock_50) then
			if  bcd_st = '1' then
				dcnt <= to_unsigned(0,5);
				sec_tmp <= '0' & seconds;
				min_tmp <= '0' & minutes;
			else
				if dcnt = 8 then
					dcnt <= dcnt;
					sec_tmp <= sec_tmp;
					min_tmp <= min_tmp;
				else
					dcnt <= dcnt + 1;
					sec_tmp <= sec_tmp(6 downto 0) & '0';
					min_tmp <= min_tmp(6 downto 0) & '0';
				end if;
			end if;
		end if;
	end process;
	
	display : SEG7_LUT_4
	port map (
		oSEG0 => HEX0_D,
		oSEG0_DP => HEX0_DP,
		oSEG1 => HEX1_D,
		oSEG1_DP => HEX1_DP,
		oSEG2 => HEX2_D,
		oSEG2_DP => HEX2_DP,
		oSEG3 => HEX3_D,
		oSEG3_DP => HEX3_DP,
		
--		iDIG => std_logic_vector(disp_cnt) --std_logic_vector(to_unsigned(X"1234",16))
		iDIG => digits
	);

	
	

end rtl;
