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
         declare
            arg1 : String := To_String(TokStr1);
            arg2 : String := To_String(TokStr2);
         begin
            if arg1 = "+" then
               Calculator.Plus(DB, CA);
            elsif arg1 = "-" then
               Calculator.Minus(DB, CA);
            elsif arg1 = "*" then
               Calculator.Multiply(DB, CA);
            elsif arg1 = "/" then
               Calculator.Divide(DB, CA);
            elsif arg1 = "push" then
               Calculator.Push(DB, CA, StringToInteger.From_String(arg2));
            elsif arg1 = "pop" then
               Calculator.Pop(DB, CA);
            elsif arg1 = "load" then
               Calculator.Load(DB, CA, VariableStore.From_String(arg2));
            elsif arg1 = "store" then
               Calculator.Store(DB, CA, VariableStore.From_String(arg2));
            elsif arg1 = "remove" then
               Calculator.Remove(DB, CA, VariableStore.From_String(arg2));
            elsif arg1 = "list" then
               Calculator.List(DB);
            else
               Put_Line("Invalid command.");
            end if;
         end;
      end if;
   end loop;
end Main;
