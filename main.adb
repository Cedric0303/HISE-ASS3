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
   CurrentPIN : PIN.PIN;
   TokStr1 : Unbounded_String;
   TokStr2 : Unbounded_String;
   DB : VariableStore.Database;
   CA : Calculator.CommandArray := (others => (VariableStore.From_String("")));

   NumCommands : Natural := 1;
   Increment : Natural := 0;

begin
   if MyCommandLine.Argument_Count /= 1 then
      Put_Line("Please supply a master PIN.");
      return;
   end if;

   Lock.Lock(CurrentPIN, MyCommandLine.Argument(1));
   VariableStore.Init(DB);

   while NumCommands in CA'Range and
     Increment < Calculator.MAX_COMMANDS - 1 loop

      CommandLineActions.PutState(Lock.IsLocked);
      CommandLineActions.ProcessLine(TokStr1, TokStr2);

      if Lock.IsLocked and To_String(TokStr1) = "unlock" then
         Lock.Unlock(CurrentPIN, To_String(TokStr2));
      elsif Lock.IsLocked and To_String(TokStr1) = "lock" then
         Put_Line("Already locked.");
      elsif not Lock.IsLocked and To_String(TokStr1) = "lock" then
         Lock.Lock(CurrentPIN, To_String(TokStr2));
      elsif not Lock.IsLocked then
         declare
            arg1 : String := To_String(TokStr1);
            arg2 : String := To_String(TokStr2);
         begin
            if arg1 = "+" and
              ((NumCommands > CA'First + 1 and NumCommands <= CA'Last) and then
              (VariableStore.Has_Variable(DB, CA(NumCommands - 2)) and
              VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) then
               Calculator.Plus(DB, CA, NumCommands, Increment);

            elsif arg1 = "-" and
              ((NumCommands > CA'First + 1 and NumCommands <= CA'Last) and then
              (VariableStore.Has_Variable(DB, CA(NumCommands - 2)) and
              VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) then
               Calculator.Minus(DB, CA, NumCommands, Increment);

            elsif arg1 = "*" and
              ((NumCommands > CA'First + 1 and NumCommands <= CA'Last) and then
              (VariableStore.Has_Variable(DB, CA(NumCommands - 2)) and
              VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) then
               Calculator.Multiply(DB, CA, NumCommands, Increment);

            elsif arg1 = "/" and
              ((NumCommands > CA'First + 1 and NumCommands <= CA'Last) and then
              (VariableStore.Has_Variable(DB, CA(NumCommands - 2)) and
              VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) then
               Calculator.Divide(DB, CA, NumCommands, Increment);

            elsif arg1 = "push" and NumCommands < CA'Last - 1 then
               Calculator.Push(DB, CA, StringToInteger.From_String(arg2), NumCommands, Increment);

            elsif arg1 = "pop" and
              ((NumCommands > CA'First and NumCommands < CA'Last - 1) and then
              VariableStore.Has_Variable(DB, CA(NumCommands - 1))) then
               Calculator.Pop(DB, CA, NumCommands);

            elsif arg1 = "load" and NumCommands < CA'Last - 1 and
              (arg2'Length <= VariableStore.Max_Variable_Length and then
              VariableStore.Has_Variable(DB, VariableStore.From_String(arg2))) then
               Calculator.Load(DB, CA, VariableStore.From_String(arg2), NumCommands, Increment);

            elsif arg1 = "store" and
              (NumCommands > CA'First + 1 and then
              VariableStore.Has_Variable(DB, CA(NumCommands - 1))) and
              arg2'Length <= VariableStore.Max_Variable_Length then
               Calculator.Store(DB, CA, VariableStore.From_String(arg2), NumCommands);

            elsif arg1 = "remove" and NumCommands > CA'First and
              (arg2'Length <= VariableStore.Max_Variable_Length and then
              VariableStore.Has_Variable(DB, VariableStore.From_String(arg2))) then
               Calculator.Remove(DB, CA, VariableStore.From_String(arg2), NumCommands);

            elsif arg1 = "list" then
               Calculator.List(DB);

            elsif arg1 = "exit" then
               exit;
            else
               Put_Line("Invalid command.");
            end if;
         end;
      end if;
   end loop;
end Main;
