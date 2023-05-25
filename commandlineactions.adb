with MyString;
with MyStringTokeniser;
with util;

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body CommandLineActions with
  SPARK_Mode
is

   procedure PutState (locked : in Boolean) is
   begin
      if locked then
         Put ("locked> ");
      else
         Put ("unlocked>   ");
      end if;
   end PutState;

   procedure ProcessLine
     (S   : in     String; command : out Unbounded_String;
      arg :    out Unbounded_String)
   is
      package Lines is new MyString (Max_MyString_Length => 2048);
      NumTokens : Natural;
      LinesString : Lines.MyString;
      CommandString : Lines.MyString;
      ArgString : Lines.MyString;

   begin
      declare
         T : MyStringTokeniser.TokenArray(1 .. 5) := (others => (Start => 1, Length => 0));
      begin
         command := Null_Unbounded_String;
         arg     := Null_Unbounded_String;

         if S'Length > 2048 then
            return;
         end if;

         MyStringTokeniser.Tokenise(S, T, NumTokens);
         LinesString := Lines.From_String(S);

         if NumTokens = 2 then
            if util.checkOverflow(T(1).Length, T(1).Start - 1) and then util.checkOverflow(T(1).Length, Lines.Length(LinesString) - T(1).Start + 1) and then T(1).Start < Lines.Length(LinesString) - T(1).Length + 1 then
               CommandString := Lines.Substring(LinesString, T (1).Start, T (1).Start + T (1).Length - 1);
               command := To_Unbounded_String(Lines.To_String(CommandString));
            end if;
            if util.checkOverflow(T(2).Length, T(2).Start - 1) and then util.checkOverflow(T(2).Length, Lines.Length(LinesString) - T(2).Start + 1) and then T(2).Start < Lines.Length(LinesString) - T(2).Length + 1 then
               ArgString := Lines.Substring(LinesString, T (2).Start, T (2).Start + T (2).Length - 1);
               arg     := To_Unbounded_String(Lines.To_String(ArgString));
            end if;
         elsif NumTokens = 1 then
            if util.checkOverflow(T(1).Length, T(1).Start - 1) and then util.checkOverflow(T(1).Length, Lines.Length(LinesString) - T(1).Start + 1) and then T(1).Start < Lines.Length(LinesString) - T(1).Length + 1 then
               CommandString := Lines.Substring(LinesString, T (1).Start, T (1).Start + T (1).Length - 1);
               command := To_Unbounded_String(Lines.To_String(CommandString));
            end if;
         else
            Put_Line ("Invalid command.");
         end if;
      end;
   end ProcessLine;

end CommandLineActions;
