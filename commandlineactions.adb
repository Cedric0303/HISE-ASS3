with MyString;
with MyStringTokeniser;

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
      package Lines is new MyString (Max_MyString_Length => 2_048);
      NumTokens : Natural;

   begin
      declare
         T : MyStringTokeniser.TokenArray (1 .. 5) :=
           (others => (Start => 1, Length => 0));
      begin
         command := Null_Unbounded_String;
         arg     := Null_Unbounded_String;

         if S'Length > 2_048 then
            return;
         end if;

         MyStringTokeniser.Tokenise (S, T, NumTokens);

         if NumTokens = 2 then
            command :=
              To_Unbounded_String
                (Lines.To_String
                   (Lines.Substring
                      (Lines.From_String (S), T (1).Start,
                       T (1).Start + T (1).Length - 1)));
            arg     :=
              To_Unbounded_String
                (Lines.To_String
                   (Lines.Substring
                      (Lines.From_String (S), T (2).Start,
                       T (2).Start + T (2).Length - 1)));
         elsif NumTokens = 1 then
            command :=
              To_Unbounded_String
                (Lines.To_String
                   (Lines.Substring
                      (Lines.From_String (S), T (1).Start,
                       T (1).Start + T (1).Length - 1)));
         else
            Put_Line ("Invalid command.");
         end if;
      end;
   end ProcessLine;

end CommandLineActions;
