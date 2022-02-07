--------------------------------------------------------------------------------------------------------------------
									 -- 16 BIT PROCESSORS--
--------------------------------------------------------------------------------------------------------------------

--Author    : Mustafa Oztoprak

--Purpose   : Design a 16 bits RISC Processor

--Variables : clk,I,Memory,IR_Register,data_mem,Instruction_mem,RAM,Ins_data
--				  result,Mux1,Mux2,Mux3,Mux4,Mux5,MRW,RFRW,PC_current,PC_next,PC_memory,a,b,c,
--				  opcode,,select1,select2,select3,cnt,ALU

--The system works with every two clock signal. 

--Opcode :
--0000 -- ADD, $R1 = $R1 + $R2
--0001 -- SUB, $R1 = $R1 - $R2
--0010 -- MUL, $R1 = $R1 * $R2
--0011 -- SLA , $R1 = $R1 << 1
--0100 -- SRA , $R1 = $R1 >> 1
--0101 -- XOR, $R1 xor $R2
--0110 -- AND, $R1 =$R1 and $R2
--0111 -- Addi, $R1 = $R1 + $R3
--1000 -- subi, $R1 = $R1 - $R3
--1001 -- LW, result = Memory[$R3 + offset]
--1010 -- SW, Memory[$R3 + offset] = result
--1011 -- beq, Branch to (PC+2) when $R1 = $R2
--1100 -- Jump



library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.all;  
use IEEE.std_logic_unsigned.all;



entity risc_16_bit is
	Port (
		 clk          : in std_logic;
		 IR_register  : in unsigned(15 downto 0)

 );
 end entity;
 
architecture Behavioral of risc_16_bit is
 ------------------------------------------------------------------------------------------------------------------
 --                                  MEMORIES
 ------------------------------------------------------------------------------------------------------------------
 type data_mem is array (0 to 255) of std_logic_vector(7 downto 0);
 type Instruction_mem is array (0 to 15) of std_logic_vector(15 downto 0);
  
 signal RAM : data_mem;
  
 signal Ins_data : Instruction_mem :=(
   "1000000110000000",
   "0010110010001011",
   "1100010000000011",
   "0001000111000000",
   "1110110110000001",
   "1100000001111011",
   "0011000011000011",
   "1111111100000000",
   "1110110110000001",
   "1000010010010101",
   "1111000000001111",
   "1010101010101010",
   "0000011111100000",
   "1100000001111011",
   "0011000011000011",
   "0010110010001011");

-----------------------------------------------------------------------------------------------------------------------
--                                   VARIABLES
-----------------------------------------------------------------------------------------------------------------------	
	
 signal ALU            : unsigned(15 downto 0);
 signal opcode         : unsigned(3 downto 0); 
 signal Mux1,Mux2,Mux3 : std_logic;
 signal MRW,RFRW       : std_logic;
 signal Mux4           : std_logic_vector(0 to 1);
 signal Mux5           : std_logic_vector(0 to 2);
 signal PC_current     : std_logic_vector(15 downto 0) :="0000000000100000";
 signal PC_next        : std_logic_vector(15 downto 0) :="0000000000100000";
 signal a              : unsigned(0 to 3) :="0000";			   
 signal b              : unsigned(0 to 3) :="0000";			   
 signal c			     : unsigned(0 to 3) :="0000";
 signal select1        : integer range 0 to 16;  
 signal select2        : integer range 0 to 16; 
 signal select3        : integer range 0 to 128;
 signal result         : std_logic_vector(0 to 31);
 signal cnt            : integer:=0;                     
 signal memory_address : unsigned(0 to 7);	
 signal offset 	     : unsigned(0 to 7);
------------------------------------------------------------------------------------------------------------------------- 
--                            VARIABLES FOR NEW FEATURES(23.12.2019 - WHILE LOOP)
------------------------------------------------------------------------------------------------------------------------ 
 signal PC_memory      : std_logic_vector(15 downto 0) :="0000000010000000"; 
 signal R0				  : std_logic_vector(0 to 15);
 signal R1				  : std_logic_vector(0 to 15);
 signal R2				  : std_logic_vector(0 to 15);
 signal andi			  : std_logic_vector(15 downto 0):="0000000000000000"; 
 begin 
 
-----------------------------------------------------------------------------------------------------------------------
--                             ASSIGNED VARIABLES
-----------------------------------------------------------------------------------------------------------------------
 process(select1,select2,select3,clk,opcode)
 begin
	a 						 <= IR_register(11 downto 8);
	b 						 <= IR_register(7 downto 4);
	c						 <= IR_register(3 downto 0);
	memory_address		 <= IR_register(7 downto 0);
	ALU(15 downto 12)  <= IR_register(15 downto 12);
	opcode(3 downto 0) <= ALU(15 downto 12);
	offset 				 <= IR_register(7 downto 0);
	select1 <= to_integer(a);
   select2 <= to_integer(b);
   select3 <= to_integer(c);

-----------------------------------------------------------------------------------------------------------------------
--                             CONTROL UNIT TABLE
-----------------------------------------------------------------------------------------------------------------------	
 case opcode(3 downto 0) is
  when "0000" => -- ADD, $R1 = $R1 + $R2
   Mux1 <= '0';
   Mux2 <= '0';
   Mux3 <= '0';
   Mux4 <= "00";
	Mux5 <= "000";
   MRW  <= '0';
   RFRW <= '1';
   result(16 to 31) <= Ins_data(select1) + Ins_data(select2);
	
  when "0001" => -- SUB, $R1 = $R1 - $R2
   Mux1 <= '0';
   Mux2 <= '0';
   Mux3 <= '0'; 
   Mux4 <= "00";
	Mux5 <= "000";
   MRW  <= '0';
   RFRW <= '1';
   result(16 to 31) <= Ins_data(select1) - Ins_data(select2);
	 
	 
  when "0010" => -- MUL, $R1 = $R1 * $R2
   Mux1 <= '0';
   Mux2 <= '0';
   Mux3 <= '0';
   Mux4 <= "00";
	Mux5 <= "000";
   MRW  <= '0';
   RFRW <= '1';
   result <= Ins_data(select1) * Ins_data(select2);
	 
  when "0011" => -- SLA , $R1 = $R1 << 1
   Mux1 <= '0';
   Mux2 <= '0';
   Mux3 <= '0'; 
   Mux4 <= "00";
	Mux5 <= "000";
   MRW  <= '0';
   RFRW <= '1';
   result(16 to 31) <= std_logic_vector(shift_left(unsigned(Ins_data(select1)),1));

  when "0100" => -- SRA, $R1 = $R1 >>1
   Mux1 <= '0';
   Mux2 <= '0';
   Mux3 <= '0'; 
   Mux4 <= "00";
	Mux5 <= "000";
   MRW  <= '0';
   RFRW <= '1';
   result(16 to 31) <= std_logic_vector(shift_right(unsigned(Ins_data(select1)),1));

  when "0101" => -- XOR, $R1 xor $R2
   Mux1 <= '0';
   Mux2 <= '0';
   Mux3 <= '0'; 
   Mux4 <= "00";
	Mux5 <= "000";
   MRW  <= '0';
   RFRW <= '1';
   result(16 to 31) <= Ins_data(select1) xor Ins_data(select2);
	 
	 
  when "0110" => -- AND, $R1 =$R1 and $R2
   Mux1 <= '0';
   Mux2 <= '0';
   Mux3 <= '0'; 
   Mux4 <= "00";
	Mux5 <= "000";
   MRW  <= '0';
   RFRW <= '1';
   result(16 to 31) <= Ins_data(select1) and Ins_data(select2);
	
	
  when "0111" =>-- Addi, $R1 = $R1 + $R3
   Mux1 <= '1';
   Mux2 <= '0';
   Mux3 <= '0'; 
   Mux4 <= "00";
	Mux5 <= "000";
   MRW  <= '0';
   RFRW <= '1';
   result(16 to 31) <= Ins_data(select1) + select3;
	
  when "1000" =>-- subi, $R1 = $R1 - $R3
   Mux1 <= '1';
   Mux2 <= '0';
   Mux3 <= '0'; 
   Mux4 <= "00";
	Mux5 <= "000";
   MRW  <= '0';
   RFRW <= '1';
   result(16 to 31) <= Ins_data(select1) - select3;

  when "1001" =>-- LW, result = Memory[$R3 + offset]
   --Mux1 <= "0";
   Mux2 <= '1';
   Mux3 <= '1';
   Mux4 <= "01";
	Mux5 <= "001";
   MRW  <= '0';
   RFRW <= '1';
   result(16 to 23) <= RAM(select1);
	result(24 to 31) <= RAM(select1+1);
	
  when "1010" => -- SW, Memory[$R3 + offset] = result
   --Mux1 <= '0';
   Mux2 <= '1';
   --Mux3 <= '0';
   Mux4 <= "01";
	Mux5 <= "001";
   MRW  <= '1';
   RFRW <= '0';
   result(16 to 31) <= Ins_data(select1);
	
  when "1011" => -- beq, Branch to (PC+2) when $R1 = $R2
   Mux1 <= '0';
   Mux2 <= '0';
   --Mux3 <= '0';
   Mux4 <= "01";
	Mux5 <= "011";
   MRW  <= '0';
   RFRW <= '0';
   

  when "1100" => -- Jump
   --Mux1 <= '0';
   --Mux2 <= '0';
   --Mux3 <= '0';
   Mux4 <= "01";
	Mux5 <= "010";
   MRW  <= '0';
   RFRW <= '0';
   
 when "1111" =>  -- While loop(23.12.2019)
	Mux5 <= "100";
	
  when others =>   
   Mux1 <= '1';
   Mux2 <= '1';
   --Mux3 <= '0';
   Mux4 <= "11";
	Mux5 <= "000";
   MRW  <= '1';
   RFRW <= '1';	

 end case;	
end process;
 

 -------------------------------------------------------------------------------------------------------------------
 --                             ASSIGNING FIRST 32 VALUES TO RAM WITH RANDOMLY
 ------------------------------------------------------------------------------------------------------------------
 process(clk)
 begin

 
 RAM(0 to 31)<=(
	"10000001","10000000","00101100","10001011","11000100","00000011","00010001","11000000","11101101","10000001",
   "11000000","01111011","00110000","11000011","11111111","00000000","11101101","10000001","10000100","10010101",
   "11110000","00001111","10101010","10101010","00000111","11100000","11000000","01111011","00110000","11000011",
   "00101100","10001011");

 --------------------------------------------------------------------------------------------------------------------
 --                                     PC CONTROL UNIT
 --------------------------------------------------------------------------------------------------------------------
 
 if(clk'event and clk='1') then

 	case Mux5 is
		when "000" =>
			PC_next<= PC_current	+ x"0002";
			PC_current <= PC_next;

		when "001" =>
			PC_next<= PC_current	+ ("00000000" & std_logic_vector(offset)); 
			PC_current <= PC_next;
			
		when "010" =>
			PC_next<= PC_current	+ ("0000" & std_logic_vector(IR_register(11 downto 0)));
			PC_current <= PC_next;
		
		when "011" =>
			PC_next<= PC_current	+ ("00000000" & std_logic_vector(IR_register(7 downto 0)));
			PC_current <= PC_next;

		when "100" =>
			PC_memory<= PC_memory;
			PC_memory<= PC_memory	+ x"0002";	
			
		when others =>
			PC_next<= PC_current	+ x"0002";
			PC_current <= PC_next;
	end case;

---------------------------------------------------------------------------------------------------------------------
--                                     ARITHMETIC OPERATIONS
---------------------------------------------------------------------------------------------------------------------

	
 	if (to_integer(unsigned(PC_current)) mod 176 = 0) then
		PC_current <= "0000000000100000" ; 
	end if;
	
	if(Mux2='0' and Mux3='0' and Mux4="00" and clk='1')then
		Ins_data(select1)<=result(16 to 31);
		RAM(to_integer(unsigned(PC_current))) <= result(16 to 23);
		RAM(to_integer(unsigned(PC_current)) + 1)<=result(24 to 31);

		
		cnt <= cnt+1;
		if(cnt mod 2 = 0) then
			RAM(to_integer(unsigned(PC_current+"0000000010010010"))) <= std_logic_vector(IR_register(15 downto 8));
			RAM(to_integer(unsigned(PC_current+"0000000010010010")) +1) <= std_logic_vector(IR_register(7 downto 0));
			
		end if;
		
		
	end if;
 end if;



--Branch
 if((IR_register(select1) = IR_register(select2)) and opcode = "1011" and clk ='1') then
	PC_current<= PC_current;

 end if;

 --Jump
 if(opcode="1100" and clk ='1') then
	PC_current <= PC_current;
 end if;
 
 --Load Word
 if(opcode ="1001" and clk ='1') then
	RAM(to_integer(unsigned(PC_current))) <= result(16 to 23);
	RAM(to_integer(unsigned(PC_current))+1) <= result(24 to 31);
end if;
	
 --Store Word
 if(opcode ="1010" and clk ='1') then
	RAM(to_integer(unsigned(PC_current)))<= result(16 to 23);
	RAM(to_integer(unsigned(PC_current))+1) <= result(24 to 31);
	
end if;

---------------------------------------------------------------------------------------------------------------------
--                                  WHILE LOOP FROM 128 TO 142 
---------------------------------------------------------------------------------------------------------------------

if(opcode ="1111" and clk='1') then

	case PC_memory is
	when "0000000001111110" =>   --126
		PC_memory <= PC_memory;	
		
	when "0000000010000000" =>    --128
		R0<= Ins_data(select1) and andi;
		RAM(to_integer(unsigned(PC_memory))) <= R0(0 to 7);
		RAM(to_integer(unsigned(PC_memory)) + 1)<=R0(8 to 15);

	
	when "0000000010000010" =>   --130
		R1<= Ins_data(select2) and andi;
		RAM(to_integer(unsigned(PC_memory))) <= R1(0 to 7);
		RAM(to_integer(unsigned(PC_memory)) + 1)<=R1(8 to 15);

	
	when "0000000010000100" =>   --132
		R2<=Ins_data(select3) and andi;
		RAM(to_integer(unsigned(PC_memory))) <= R2(0 to 7);
		RAM(to_integer(unsigned(PC_memory)) + 1)<=R2(8 to 15);

		
	when "0000000010000110" =>   --134
		R0 <= R0 + x"0005";
		RAM(to_integer(unsigned(PC_memory))) <= R0(0 to 7);
		RAM(to_integer(unsigned(PC_memory)) + 1)<=R0(8 to 15);
		
		
	when "0000000010001000" =>   --136
		if(R1 = R0 ) then		
			PC_memory <= PC_memory +x"0008";
			
		else
			PC_memory<=PC_memory + x"0002";
			
		end if;

	when "0000000010001010" =>   --138
		R2 <= R2 + x"0001";
		RAM(to_integer(unsigned(PC_memory))) <= R2(0 to 7);
		RAM(to_integer(unsigned(PC_memory)) + 1)<=R2(8 to 15);

		
	when "0000000010001100" =>   --140
		R0 <= R0 - x"0001";
		RAM(to_integer(unsigned(PC_memory))) <= R0(0 to 7);
		RAM(to_integer(unsigned(PC_memory)) + 1)<=R0(8 to 15);

		 
	when "0000000010001110" =>  --142
		PC_memory <= PC_memory - x"0006";
	
	when others =>
		PC_memory <= PC_memory;

end case;
end if;
end process;	
end Behavioral;

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 