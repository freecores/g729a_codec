-----------------------------------------------------------------
--                                                             --
-----------------------------------------------------------------
--                                                             --
-- Copyright (C) 2013 Stefano Tonello                          --
--                                                             --
-- This source file may be used and distributed without        --
-- restriction provided that this copyright statement is not   --
-- removed from the file and that any derivative work contains --
-- the original copyright notice and the associated disclaimer.--
--                                                             --
-- THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY         --
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED   --
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS   --
-- FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL THE AUTHOR      --
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,         --
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    --
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE   --
-- GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR        --
-- BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF  --
-- LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY, OR TORT  --
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT  --
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         --
-- POSSIBILITY OF SUCH DAMAGE.                                 --
--                                                             --
-----------------------------------------------------------------

---------------------------------------------------------------
-- G.729a ASIP 16x16 Register File
---------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library work;
use work.G729A_ASIP_PKG.all;

entity G729A_ASIP_REGFILE_16X16_2W is
  port(
    CLK_i : in std_logic;
    RA0_i : in RID_T;
    RA1_i : in RID_T;
    RA2_i : in RID_T;
    RA3_i : in RID_T;
    WA0_i : in RID_T;
    WA1_i : in RID_T;
    LR0_i : in std_logic;
    LR1_i : in std_logic;
    LR2_i : in std_logic;
    LR3_i : in std_logic;
    LW0_i : in std_logic;
    LW1_i : in std_logic;
    WE0_i : in std_logic;
    WE1_i : in std_logic;
    D0_i : in std_logic_vector(LDLEN-1 downto 0);
    D1_i : in std_logic_vector(LDLEN-1 downto 0);

    Q0_o : out std_logic_vector(LDLEN-1 downto 0);
    Q1_o : out std_logic_vector(LDLEN-1 downto 0);
    Q2_o : out std_logic_vector(LDLEN-1 downto 0);
    Q3_o : out std_logic_vector(LDLEN-1 downto 0)
  );
end G729A_ASIP_REGFILE_16X16_2W;

architecture ARC of G729A_ASIP_REGFILE_16X16_2W is

  constant REGNUM : natural := 16;
  constant ZERO : std_logic_vector(SDLEN-1 downto 0) := (others => '0');

  subtype WORD_T is std_logic_vector(SDLEN-1 downto 0);
  type MEM_T is array (REGNUM/2-1 downto 0) of WORD_T;

  signal REG_EVEN,REG_ODD : MEM_T;
  signal WE0_EVEN,WE0_ODD : std_logic;
  signal WE1_EVEN,WE1_ODD : std_logic;
  signal IWA0,IWA1 : natural range 0 to REGNUM/2-1;
  signal WA0_LSB,WA1_LSB : std_logic;
  signal IRA0,IRA1,IRA2,IRA3 : natural range 0 to REGNUM/2-1;
  signal RA0_LSB,RA1_LSB,RA2_LSB,RA3_LSB : std_logic;
  signal D0_LO,D0_HI : std_logic_vector(SDLEN-1 downto 0);
  signal D0_EVEN,D0_ODD : std_logic_vector(SDLEN-1 downto 0);
  signal D1_LO,D1_HI : std_logic_vector(SDLEN-1 downto 0);
  signal D1_EVEN,D1_ODD : std_logic_vector(SDLEN-1 downto 0);
  signal Q0_EVEN,Q1_EVEN : std_logic_vector(SDLEN-1 downto 0);
  signal Q0_ODD,Q1_ODD : std_logic_vector(SDLEN-1 downto 0);
  signal Q2_EVEN,Q3_EVEN : std_logic_vector(SDLEN-1 downto 0);
  signal Q2_ODD,Q3_ODD : std_logic_vector(SDLEN-1 downto 0);

  function GET_LSB(N : natural range 0 to 16-1) return std_logic is
    variable U : unsigned(4-1 downto 0);
  begin
    U := to_unsigned(N,4); 
    return(U(0));
  end function;

begin

  ---------------------------------------------

  D0_LO <= D0_i(SDLEN-1 downto 0);
  D0_HI <= D0_i(SDLEN*2-1 downto SDLEN);

  D0_EVEN <= D0_LO;
  D0_ODD <= D0_HI when (LW0_i = '1') else D0_LO;

  process(WA0_LSB,WE0_i,LW0_i)
  begin
    if(LW0_i = '0') then
      WE0_EVEN <= (WE0_i and not(WA0_LSB));
      WE0_ODD <= (WE0_i and WA0_LSB);
    else
      WE0_EVEN <= WE0_i;
      WE0_ODD <= WE0_i;
    end if;
  end process;

  ---------------------------------------------

  D1_LO <= D1_i(SDLEN-1 downto 0);
  D1_HI <= D1_i(SDLEN*2-1 downto SDLEN);

  D1_EVEN <= D1_LO;
  D1_ODD <= D1_HI when (LW1_i = '1') else D1_LO;

  process(WA1_LSB,WE1_i,LW1_i)
  begin
    if(LW1_i = '0') then
      WE1_EVEN <= (WE1_i and not(WA1_LSB));
      WE1_ODD <= (WE1_i and WA1_LSB);
    else
      WE1_EVEN <= WE1_i;
      WE1_ODD <= WE1_i;
    end if;
  end process;

  ---------------------------------------------

  IWA0 <= WA0_i/2;
  IWA1 <= WA1_i/2;

  WA0_LSB <= GET_LSB(WA0_i);
  WA1_LSB <= GET_LSB(WA1_i);

  process(CLK_i)
  begin
    if(CLK_i = '1' and CLK_i'event) then
      if(WE0_EVEN = '1') then
        REG_EVEN(IWA0) <= D0_EVEN;
      end if;
      if(WE1_EVEN = '1') then
        REG_EVEN(IWA1) <= D1_EVEN;
      end if;
    end if;
  end process;

  process(CLK_i)
  begin
    if(CLK_i = '1' and CLK_i'event) then
      if(WE0_ODD = '1') then
        REG_ODD(IWA0) <= D0_ODD;
      end if;
      if(WE1_ODD = '1') then
        REG_ODD(IWA1) <= D1_ODD;
      end if;
    end if;
  end process;

  ---------------------------------------------

  IRA0 <= RA0_i/2;
  IRA1 <= RA1_i/2;
  IRA2 <= RA2_i/2;
  IRA3 <= RA3_i/2;

  RA0_LSB <= GET_LSB(RA0_i);
  RA1_LSB <= GET_LSB(RA1_i);
  RA2_LSB <= GET_LSB(RA2_i);
  RA3_LSB <= GET_LSB(RA3_i);

  Q0_EVEN <= REG_EVEN(IRA0);
  Q1_EVEN <= REG_EVEN(IRA1);
  Q2_EVEN <= REG_EVEN(IRA2);
  Q3_EVEN <= REG_EVEN(IRA3);

  Q0_ODD <= REG_ODD(IRA0);
  Q1_ODD <= REG_ODD(IRA1);
  Q2_ODD <= REG_ODD(IRA2);
  Q3_ODD <= REG_ODD(IRA3);

  process(RA0_LSB,LR0_i,Q0_EVEN,Q0_ODD)
  begin
    if(LR0_i = '0' and RA0_LSB = '1') then
      Q0_o(SDLEN-1 downto 0) <= Q0_ODD;
    else
      Q0_o(SDLEN-1 downto 0) <= Q0_EVEN;
    end if;
    if(LR0_i = '0') then
      Q0_o(SDLEN*2-1 downto SDLEN) <= (others => '0');
    else
      Q0_o(SDLEN*2-1 downto SDLEN) <= Q0_ODD;
    end if;
  end process;

  process(RA1_LSB,LR1_i,Q1_EVEN,Q1_ODD)
  begin
    if(LR1_i = '0' and RA1_LSB = '1') then
      Q1_o(SDLEN-1 downto 0) <= Q1_ODD;
    else
      Q1_o(SDLEN-1 downto 0) <= Q1_EVEN;
    end if;
    if(LR1_i = '0') then
      Q1_o(SDLEN*2-1 downto SDLEN) <= (others => '0');
    else
      Q1_o(SDLEN*2-1 downto SDLEN) <= Q1_ODD;
    end if;
  end process;

  process(RA2_LSB,LR2_i,Q2_EVEN,Q2_ODD)
  begin
    if(LR2_i = '0' and RA2_LSB = '1') then
      Q2_o(SDLEN-1 downto 0) <= Q2_ODD;
    else
      Q2_o(SDLEN-1 downto 0) <= Q2_EVEN;
    end if;
    if(LR2_i = '0') then
      Q2_o(SDLEN*2-1 downto SDLEN) <= (others => '0');
    else
      Q2_o(SDLEN*2-1 downto SDLEN) <= Q2_ODD;
    end if;
  end process;

  process(RA3_LSB,LR3_i,Q3_EVEN,Q3_ODD)
  begin
    if(LR3_i = '0' and RA3_LSB = '1') then
      Q3_o(SDLEN-1 downto 0) <= Q3_ODD;
    else
      Q3_o(SDLEN-1 downto 0) <= Q3_EVEN;
    end if;
    if(LR3_i = '0') then
      Q3_o(SDLEN*2-1 downto SDLEN) <= (others => '0');
    else
      Q3_o(SDLEN*2-1 downto SDLEN) <= Q3_ODD;
    end if;
  end process;

end ARC;