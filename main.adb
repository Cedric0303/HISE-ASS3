pragma SPARK_Mode (On);

with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with Calculator; use Calculator;
with PIN;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


with Ada.Long_Long_Integer_Text_IO;

procedure Main is
   DB : VariableStore.Database;
   LastCommands : VariableArray;
   Unlocked : Boolean := false;
   Done : Boolean := false;
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;
   PIN1 : PIN.PIN;
   PIN2 : PIN.PIN;
   NumTokens : Natural;
   TokStr1 : Unbounded_String;
   TokStr2 : Unbounded_String;
begin
   if MyCommandLine.Argument_Count = 1 then
      VariableStore.Init(DB);

      if MyCommandLine.Argument(1)'Length /= 4 then
         Put_Line("Invalid PIN.");
         return;
      end if;

      for I in MyCommandLine.Argument(1)'Range loop

         if MyCommandLine.Argument(1)(I) < '0' or MyCommandLine.Argument(1)(I) > '9' then
            Put_Line("Invalid PIN.");
            return;
         end if;

         pragma Loop_Invariant (for all J in MyCommandLine.Argument(1)'First..I => (MyCommandLine.Argument(1)(J) >= '0' and MyCommandLine.Argument(1)(J) <= '9'));

      end loop;

      PIN1 := PIN.From_String(MyCommandLine.Argument(1));
      while not Done loop
         if Unlocked then
            Put("unlocked> ");
         else
            Put("locked>   ");
         end if;
         Lines.Get_Line(S);
         declare
            T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
         begin
            MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);

            if NumTokens = 2 then
               TokStr1 := To_Unbounded_String(Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)));
               TokStr2 := To_Unbounded_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
            elsif NumTokens = 1 then
               TokStr1 := To_Unbounded_String(Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)));
               TokStr2 := Null_Unbounded_String;
            else
               Put_Line("Invalid command.");
               return;
            end if;

            if not Unlocked then
               if To_String(TokStr1) = "unlock" then
                  PIN2 := PIN.From_String(To_String(TokStr2));
                  if PIN."="(PIN1,PIN2) then
                     Put_Line("Calculator unlocked.");
                     Unlocked := True;
                  else
                     Put_Line("Wrong PIN.");
                  end if;
               else
                  Put_Line("Calculator locked.");
               end if;
            elsif To_String(TokStr1) = "lock" then
               PIN1 := PIN.From_String(To_String(TokStr2));
               Unlocked := False;
               Put_Line("Calculator locked.");
            else
               Calculator.Process(DB, LastCommands, To_String(TokStr1), To_String(TokStr2), Done);
            end if;
         end;
      end loop;
   else
      Put_Line("Please supply a master PIN.");
      return;
   end if;
end Main;
