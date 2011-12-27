library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_arith.all;
  use ieee.std_logic_unsigned.all;
use ieee.numeric_std.ALL;

entity i2c_sim is
end i2c_sim;

architecture tb of i2c_sim is

signal CLK : std_logic;
signal OCLK : std_logic;
signal reset : std_logic;
signal addr : std_logic_vector(2 downto 0);
signal din : std_logic_vector(7 downto 0);
signal dout : std_logic_vector(7 downto 0);
signal we : std_logic;
signal stb : std_logic;
signal cyc : std_logic;
signal ack : std_logic;
signal scl : std_logic;
signal sda : std_logic;
signal scl_pad_o : std_logic;
signal scl_padoen_o : std_logic;
signal scl_pad_i : std_logic;
signal sda_pad_o : std_logic;
signal sda_padoen_o : std_logic;
signal sda_pad_i : std_logic;
signal adr	: std_logic_vector(2 downto 0);
signal d	: std_logic_vector(7 downto 0);
component i2c_master_top is
    generic(
            ARST_LVL : std_logic := '0'                   -- asynchronous reset level
    );
    port   (
            -- wishbone signals
            wb_clk_i      : in  std_logic;                    -- master clock input
            wb_rst_i      : in  std_logic := '0';             -- synchronous active high reset
            arst_i        : in  std_logic := not ARST_LVL;    -- asynchronous reset
            wb_adr_i      : in  std_logic_vector(2 downto 0); -- lower address bits
            wb_dat_i      : in  std_logic_vector(7 downto 0); -- Databus input
            wb_dat_o      : out std_logic_vector(7 downto 0); -- Databus output
            wb_we_i       : in  std_logic;                    -- Write enable input
            wb_stb_i      : in  std_logic;                    -- Strobe signals / core select signal
            wb_cyc_i      : in  std_logic;                    -- Valid bus cycle input
            wb_ack_o      : out std_logic;                    -- Bus cycle acknowledge output
            wb_inta_o     : out std_logic;                    -- interrupt request output signal

            -- i2c lines
            scl_pad_i     : in  std_logic;                    -- i2c clock line input
            scl_pad_o     : out std_logic;                    -- i2c clock line output
            scl_padoen_o  : out std_logic;                    -- i2c clock line output enable, active low
            sda_pad_i     : in  std_logic;                    -- i2c data line input
            sda_pad_o     : out std_logic;                    -- i2c data line output
            sda_padoen_o  : out std_logic                     -- i2c data line output enable, active low
    );
end component i2c_master_top;


begin
	u0 : i2c_master_top
	port map(
            -- wishbone signals
            wb_clk_i	=> CLK,
            wb_rst_i	=> reset,
--            arst_i	=> reset_n,
            wb_adr_i	=> addr,
            wb_dat_i	=> din,
            wb_dat_o	=> dout,
            wb_we_i	=> we,
            wb_stb_i	=> stb,
            wb_cyc_i	=> cyc,
            wb_ack_o	=> ack,
--            wb_inta_o	=>

            -- i2c lines
            scl_pad_i	=> scl_pad_i,
            scl_pad_o	=> scl_pad_o,
            scl_padoen_o	=> scl_padoen_o,
            sda_pad_i	=> sda_pad_i,
            sda_pad_o	=> sda_pad_o,
            sda_padoen_o	=> sda_padoen_o
	);
	
	scl <= scl_pad_o when (scl_padoen_o = '0') else 'Z';
	sda <= sda_pad_o when (sda_padoen_o = '0') else 'Z';
	scl_pad_i <= scl;
	scl_pad_i <= sda;

	
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
		procedure wb_write (
			signal adr : in std_logic_vector(2 downto 0); 
			signal d : in std_logic_vector(7 downto 0)
			) is
		begin
			wait until rising_edge(clk);
			addr <= adr;
			dout <= d;
			cyc <= '1';
			stb <= '1';
			we <= '1';
			wait until rising_edge(clk);
			wait until falling_edge(ack);
			addr <= "000";
			dout <= "00000000";
			cyc <= '0';
			stb <= '0';
			we <= '0';
		end procedure wb_write;
		
	begin
		wait until falling_edge(reset);
		d <= "00000000";
		adr <= "000";
		wb_write(adr,d);
--		wb_write("000","00000000");
--		wb_write("010","00000000");
--		wb_write("010","10000000");
--		wb_write("011","10101010");
--		wb_write("100","10000000");
--		wb_write("100","00010000");
--		wb_write("100","01000000");		
	end process;
end tb;