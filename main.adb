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
   CurrentPIN : PIN.PIN := PIN.From_String("0000");
   TokStr1 : Unbounded_String;
   TokStr2 : Unbounded_String;
   DB : VariableStore.Database;
   CA : Calculator.CommandArray := (others => (VariableStore.From_String("")));

begin
   if MyCommandLine.Argument_Count /= 1 then
      Put_Line("Please supply a master PIN.");
      return;
   end if;

   Lock.Lock(CurrentPIN, MyCommandLine.Argument(1));
   VariableStore.Init(DB);

   while True loop
      CommandLineActions.PutState(Lock.IsLocked);
      CommandLineActions.ProcessLine(TokStr1, TokStr2);

      if Lock.IsLocked and To_String(TokStr1) = "unlock" then
         Lock.Unlock(CurrentPIN, To_String(TokStr2));
      elsif not Lock.IsLocked and To_String(TokStr1) = "lock" then
         Lock.Lock(CurrentPIN, To_String(TokStr2));
      elsif not Lock.IsLocked then
         Calculator.Process(DB, CA, To_String(TokStr1), To_String(TokStr2));
      end if;
   end loop;
end Main;
