-- Z80, Monitor ROM, 4k RAM and two 16450 UARTs
-- that can be synthesized and used with
-- the NoICE debugger that can be found at
-- http://www.noicedebugger.com/

library IEEE;
use IEEE.std_logic_1164.all;

entity DebugSystem is
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
end DebugSystem;

architecture struct of DebugSystem is

	signal M1_n			: std_logic;
	signal MREQ_n		: std_logic;
	signal IORQ_n		: std_logic;
	signal RD_n			: std_logic;
	signal WR_n			: std_logic;
	signal RFSH_n		: std_logic;
	signal HALT_n		: std_logic;
	signal WAIT_n		: std_logic;
	signal INT_n		: std_logic;
	signal RESET_s		: std_logic;
	signal BUSRQ_n		: std_logic;
	signal BUSAK_n		: std_logic;
	signal A			: std_logic_vector(15 downto 0);
	signal D			: std_logic_vector(7 downto 0);
	signal ROM_D		: std_logic_vector(7 downto 0);
	signal SRAM_D		: std_logic_vector(7 downto 0);
	signal UART0_D		: std_logic_vector(7 downto 0);
	signal UART1_D		: std_logic_vector(7 downto 0);
	signal CPU_D		: std_logic_vector(7 downto 0);

	signal Mirror		: std_logic;

	signal IOWR_n		: std_logic;
	signal RAMCS_n		: std_logic;
	signal UART0CS_n	: std_logic;
	signal UART1CS_n	: std_logic;
	signal UART0CS		: std_logic;
	signal UART1CS		: std_logic;

	signal BaudOut0		: std_logic;
	signal BaudOut1		: std_logic;
	
	signal SRAM2_D		: std_logic_vector(7 downto 0);
	signal SRAM2CS_n	: std_logic;

	signal Reset		: std_logic;
	signal IOWR			: std_logic;

	signal scl_i   	: std_logic;
	signal scl_o   	: std_logic;
	signal scl_oe	: std_logic;
	signal sda_i   	: std_logic;
	signal sda_o   	: std_logic;
	signal sda_oe	: std_logic;
		
begin

	Wait_n <= '1';
	BusRq_n <= '1';
	INT_n <= '1';

	process (Reset_n, Clk)
	begin
		if Reset_n = '0' then
			Reset_s <= '0';
			Mirror <= '0';
		elsif Clk'event and Clk = '1' then
			Reset_s <= '1';
			if IORQ_n = '0' and A(7 downto 4) = "1111" then
				Mirror <= D(0);
			end if;
		end if;
	end process;

	IOWR_n <= WR_n or IORQ_n;
	RAMCS_n <= (not Mirror and not A(15)) or MREQ_n;
	SRAM2CS_n <= '0' when MREQ_n = '0' and A(15 downto 11) = "00110" else '1';
	UART0CS_n <= '0' when IORQ_n = '0' and A(7 downto 3) = "00000" else '1';
	UART1CS_n <= '0' when IORQ_n = '0' and A(7 downto 3) = "10000" else '1';
	
	uart0_cs_out <= uart0cs_n;
	
	CPU_D <=
		SRAM_D when RAMCS_n = '0' else
		UART0_D when UART0CS_n = '0' else
		UART1_D when UART1CS_n = '0' else
		SRAM2_D when SRAM2CS_n = '0' else
		ROM_D;

	u0 : entity work.T80s
			generic map(Mode => 1, T2Write => 1, IOWait => 0)
			port map(
				RESET_n => RESET_s,
				CLK_n => Clk,
				WAIT_n => WAIT_n,
				INT_n => INT_n,
				NMI_n => NMI_n,
				BUSRQ_n => BUSRQ_n,
				M1_n => M1_n,
				MREQ_n => MREQ_n,
				IORQ_n => IORQ_n,
				RD_n => RD_n,
				WR_n => WR_n,
				RFSH_n => RFSH_n,
				HALT_n => HALT_n,
				BUSAK_n => BUSAK_n,
				A => A,
				DI => CPU_D,
				DO => D);

	u1 : entity work.MonZ80
			port map(
				clock => Clk,
				address => A(11 downto 0),
				q => ROM_D);

	u2 : entity work.SSRAM
			generic map(
				AddrWidth => 12)
			port map(
				Clk => Clk,
				CE_n => RAMCS_n,
				WE_n => WR_n,
				A => A(11 downto 0),
				DIn => D,
				DOut => SRAM_D);

	sram : entity work.SSRAM
			generic map(
				AddrWidth => 12)
			port map(
				Clk => Clk,
				CE_n => SRAM2CS_n,
				WE_n => WR_n,
				A => A(11 downto 0),
				DIn => D,
				DOut => SRAM2_D);
	
--	sram : entity work.sram
--			port map(
--				address => A(10 downto 0),	
--				clock => Clk,
--				data => D,
--				wren => not (SRAM2CS_n or WR_n),
--				q => SRAM2_D
--			);
		
	u3 : entity work.gh_uart_16550
	port map(
		clk => Clk,
		BR_clk  => Clk,
		rst => Reset,
		CS => UART0CS,
		WR => IOWR,
		ADD => a(2 downto 0),
		D => D,
		
		sRX	=> RXD0,   
		CTSn => CTS0,
		DSRn => DSR0,
		RIn => RI0,
		DCDn => DCD0,
		
		sTX => TXD0,
		DTRn => DTR0,
		RTSn => RTS0,
		OUT1n => open,
		OUT2n => open,
		TXRDYn => open,
		RXRDYn => open,
		
		IRQ => open,
		B_CLK => BaudOut0,
		RD => UART0_D 
	);
	
--	u3 : entity work.T16450
--			port map(
--				MR_n => Reset_s,
--				XIn => Clk,
--				RClk => BaudOut0,
--				CS_n => UART0CS_n,
--				Rd_n => RD_n, 
--				Wr_n => IOWR_n,
--				A => A(2 downto 0),
--				D_In => D,
--				D_Out => UART0_D,
--				SIn => RXD0,
--				CTS_n => CTS0,
--				DSR_n => DSR0,
--				RI_n => RI0,
--				DCD_n => DCD0,
--				SOut => TXD0,
--				RTS_n => RTS0,
--				DTR_n => DTR0,
--				OUT1_n => open,
--				OUT2_n => open,
--				BaudOut => BaudOut0,
--				Intr => open);

	IOWR <= not IOWR_n;
	Reset <= not Reset_s;
	UART1CS <= not UART1CS_n;
	UART0CS <= not UART0CS_n;
	
	u4 : entity work.gh_uart_16550
	port map(
		clk => Clk,
		BR_clk  => Clk,
		rst => Reset,
		CS => UART1CS,
		WR => IOWR,
		ADD => a(2 downto 0),
		D => D,
		
		sRX	=> RXD1,   
		CTSn => CTS1,
		DSRn => DSR1,
		RIn => RI1,
		DCDn => DCD1,
		
		sTX => TXD1,
		DTRn => DTR1,
		RTSn => RTS1,
		OUT1n => open,
		OUT2n => open,
		TXRDYn => open,
		RXRDYn => open,
		
		IRQ => open,
		B_CLK => BaudOut1,
		RD => UART1_D 
	);

--	u4 : entity work.T16450
--			port map(
--				MR_n => Reset_s,
--				XIn => Clk,
--				RClk => BaudOut1,
--				CS_n => UART1CS_n,
--				Rd_n => RD_n, 
--				Wr_n => IOWR_n,
--				A => A(2 downto 0),
--				D_In => D,
--				D_Out => UART1_D,
--				SIn => RXD1,
--				CTS_n => CTS1,
--				DSR_n => DSR1,
--				RI_n => RI1,
--				DCD_n => DCD1,
--				SOut => TXD1,
--				RTS_n => RTS1,
--				DTR_n => DTR1,
--				OUT1_n => open,
--				OUT2_n => open,
--				BaudOut => BaudOut1,
--				Intr => open);

--	u5 : entity work.i2c_master_top
--    port map   (
--            -- wishbone signals
--            wb_clk_i => Clk,      
--            wb_rst_i => Reset, 
--            arst_i => Reset,
--            wb_adr_i => a(2 downto 0),     
--            wb_dat_i => d,
----            wb_dat_o
--            wb_we_i => IOWR_n,
--            wb_stb_i => '1',
--            wb_cyc_i => '1',
----            wb_ack_o      
----            wb_inta_o     
--
--            -- i2c lines
--            scl_pad_i => scl_i,
--            scl_pad_o => scl_o,
--            scl_padoen_o => scl_oe,
--            sda_pad_i => sda_i,
--            sda_pad_o => sda_o,
--            sda_padoen_o => sda_oe
--    );

end;
