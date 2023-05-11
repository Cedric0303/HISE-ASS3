pragma SPARK_Mode (On);

with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyStringTokeniser;
with PIN;

with Calculator;
with Lock;
with CommandLineActions;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is
   TokStr1 : Unbounded_String;
   TokStr2 : Unbounded_String;

begin
   if MyCommandLine.Argument_Count /= 1 then
      Put_Line("Please supply a master PIN.");
      return;
   end if;

   Lock.lock(MyCommandLine.Argument(1));
   Calculator.Init;

   while True loop
      CommandLineActions.PutState(Lock.IsLocked);
      
      declare
         T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));

      begin
         CommandLineActions.ProcessLine(T, TokStr1, TokStr2);

         if Lock.IsLocked and To_String(TokStr1) = "unlock" then
            Lock.Unlock(To_String(TokStr2));
         elsif not Lock.IsLocked and To_String(TokStr1) = "lock" then
            Lock.Lock(To_String(TokStr2));
         elsif not Lock.IsLocked then
            Calculator.Process(To_String(TokStr1), To_String(TokStr2));
         end if;
      end;
   end loop;
end Main;
