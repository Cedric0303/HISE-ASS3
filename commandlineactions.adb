with MyString;
with MyStringTokeniser;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body CommandLineActions is

   procedure PutState(locked : in Boolean) is
   begin
      if locked then
         Put("locked> ");
      else
         Put("unlocked>   ");
      end if;
   end PutState;

   procedure ProcessLine(command : out Unbounded_String; 
                         arg : out Unbounded_String) is
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;
   NumTokens : Natural;

   begin
      declare
         T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
      begin
         Lines.Get_Line(S);
         MyStringTokeniser.Tokenise(Lines.To_String(S), T, NumTokens);

         if NumTokens = 2 then
            command := To_Unbounded_String(Lines.To_String(Lines.Substring(S, T(1).Start, T(1).Start+T(1).Length-1)));
            arg := To_Unbounded_String(Lines.To_String(Lines.Substring(S, T(2).Start, T(2).Start+T(2).Length-1)));
         elsif NumTokens = 1 then
            command := To_Unbounded_String(Lines.To_String(Lines.Substring(S, T(1).Start, T(1).Start+T(1).Length-1)));
            arg := Null_Unbounded_String;
         else
            Put_Line("Invalid command.");
         end if;
      end;
   end ProcessLine;

end CommandLineActions;
