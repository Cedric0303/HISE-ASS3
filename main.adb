pragma SPARK_Mode (On);

with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyStringTokeniser;
with PIN;
with Calculator;
with Lock;
with CommandLineActions;
with MyString;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;

procedure Main is
   package Lines is new MyString(Max_MyString_Length => 2048);
   S : Lines.MyString;
   CurrentPIN : PIN.PIN;
   TokStr1 : Unbounded_String;
   TokStr2 : Unbounded_String;
   ValueStack : VariableStore.Database;
   VariableStack : VariableStore.Database;

begin
   if MyCommandLine.Argument_Count /= 1 then
      Put_Line("Please supply a master PIN.");
      return;
   end if;

   if Lock.IsInvalidPIN(MyCommandLine.Argument(1)) then
      Put_Line("Invalid PIN.");
      return;
   end if;

   Lock.Lock(CurrentPIN, MyCommandLine.Argument(1));
   VariableStore.Init(ValueStack);
   VariableStore.Init(VariableStack);

   while True loop

      CommandLineActions.PutState(Lock.IsLocked);
      Lines.Get_Line(S);
      CommandLineActions.ProcessLine(Lines.To_String(S), TokStr1, TokStr2);
      
      declare
            arg1 : String := To_String(TokStr1);
            arg2 : String := To_String(TokStr2);
      begin

         if Lock.IsLocked and arg1 = "unlock" then
            if Lock.IsInvalidPIN(arg2) then
               Put_Line("Invalid PIN.");
               return;
            end if;

            Lock.Unlock(CurrentPIN, arg2);
         elsif Lock.IsLocked and arg1 = "lock" then
            Put_Line("Already locked.");
         elsif not Lock.IsLocked and arg1 = "lock" then
            if Lock.IsInvalidPIN(arg2) then
               Put_Line("Invalid PIN.");
               return;
            end if;

            Lock.Lock(CurrentPIN, arg2);
         elsif not Lock.IsLocked then

            if arg1 = "+" and
              Integer(VariableStore.Length(ValueStack)) > 2 and
              VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 2)'Image)) and
              VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 1)'Image))
            then
               Calculator.Plus(ValueStack);

            elsif arg1 = "-" and
              Integer(VariableStore.Length(ValueStack)) > 2 and
              VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 2)'Image)) and
              VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 1)'Image))
            then
               Calculator.Minus(ValueStack);

            elsif arg1 = "*" and
              Integer(VariableStore.Length(ValueStack)) > 2 and
              VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 2)'Image)) and
              VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 1)'Image))
            then
               Calculator.Multiply(ValueStack);

            elsif arg1 = "/" and
              Integer(VariableStore.Length(ValueStack)) > 2 and
              VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 2)'Image)) and
              VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 1)'Image))
            then
               Calculator.Divide(ValueStack);

            elsif arg1 = "push" and
              (VariableStore.Length(ValueStack) < 512 or
              VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)))'Image)))
            then
               Calculator.Push(ValueStack, StringToInteger.From_String(arg2));

            elsif arg1 = "pop" and
              VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 1)'Image))
            then
               Calculator.Pop(ValueStack);

            elsif arg1 = "load" and
              (arg2'Length <= VariableStore.Max_Variable_Length and then
              (VariableStore.Has_Variable(VariableStack, VariableStore.From_String(arg2)) and
              (VariableStore.Length(ValueStack) < 512 or
              VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)))'Image)))))
            then
               Calculator.Load(ValueStack, VariableStack, VariableStore.From_String(arg2));

            elsif arg1 = "store" and
              ((arg2'Length <= VariableStore.Max_Variable_Length and
              VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 1)'Image))) and then
              (VariableStore.Length(VariableStack) < VariableStore.Max_Entries or VariableStore.Has_Variable(VariableStack, VariableStore.From_String(arg2))))
            then
               Calculator.Store(ValueStack, VariableStack, VariableStore.From_String(arg2));

            elsif arg1 = "remove" and
              (arg2'Length <= VariableStore.Max_Variable_Length and then
              VariableStore.Has_Variable(VariableStack, VariableStore.From_String(arg2)))
            then
               Calculator.Remove(VariableStack, VariableStore.From_String(arg2));

            elsif arg1 = "list" then
               Calculator.List(VariableStack);

            elsif arg1 = "exit" then
               exit;

            elsif (arg1 = "store" or arg1 = "load" or arg1 = "remove") and arg2'Length > VariableStore.Max_Variable_Length then
               Put_Line("Variable Name Too Long.");
               return;

            else
               Put_Line("Invalid command.");
               return;
               
            end if;
         end if;
      end;
   end loop;
end Main;
