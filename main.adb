-- Names and ids of you and your partner:
-- Jun Li Chen 1043258, Emmanuel Pinca 1080088

-- TO REFORMAT
-- Currently proved (complete list including security properties for task 4):
-- Stack is not empty and has a length of at least 2 when arithmetic operations are performed
-- Push is performed when the stack is not full
-- Pop is performed when stack has a value?
-- Load is performed when store has var and stack is not full
-- Store is performed stack has value, store is not full or already has value stored (to remove)
-- Remove is performed if variable exists in store
-- Unlock is only performed from the locked state
-- Unlock removes lock if pins match and vice versa
-- Lock updates the master PIN when performed

-- To prove:
-- Operations (except for unlock) are performed only when unlocked
-- Input lines longer than 2048 characters are invalid
-- Variable names longer than 1024 characters are invalid

-- To do:
-- Move print to stdout into main? (side-effects)
-- Determine max store size

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

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;        use Ada.Containers;

-- Main Function
procedure Main is
   package Lines is new MyString (Max_MyString_Length => 2048);
   S             : Lines.MyString;
   CurrentPIN    : PIN.PIN;
   TokStr1       : Unbounded_String;
   TokStr2       : Unbounded_String;
   ValueStack    : VariableStore.Database;
   VariableStack : VariableStore.Database;
   IsLocked      : Boolean := False;

begin
   -- Initial Arguments
   if MyCommandLine.Argument_Count /= 1 then
      Put_Line("Please supply a valid master PIN.");
      return;
   end if;

   if Lock.IsInvalidPIN(MyCommandLine.Argument(1)) then
      Put_Line("Invalid PIN.");
      return;
   end if;

   -- Initialisation
   Lock.Lock(CurrentPIN, MyCommandLine.Argument(1), IsLocked);
   VariableStore.Init(ValueStack);
   VariableStore.Init(VariableStack);

   loop
      -- Print current state and read current line
      CommandLineActions.PutState(IsLocked);
      Lines.Get_Line(S);
      CommandLineActions.ProcessLine(Lines.To_String(S), TokStr1, TokStr2);

      declare
         arg1 : String := To_String (TokStr1);
         arg2 : String := To_String (TokStr2);
      begin
         -- Lock operations - no effect when state matches lock state
         if arg1 = "unlock" then
            if IsLocked then
               if Lock.IsInvalidPIN (arg2) then
                  Put_Line ("Invalid PIN.");
                  return;
               end if;

               Lock.Unlock (CurrentPIN, arg2, IsLocked);

               if IsLocked then
                  Put_Line("Incorrect PIN.");
               end if;
            else
               Put_Line("Already Unlocked.");
            end if;
         elsif arg1 = "lock" then
            if not IsLocked then
               if Lock.IsInvalidPIN (arg2) then
                  Put_Line ("Invalid PIN.");
                  return;
               end if;

               Lock.Lock (CurrentPIN, arg2, IsLocked);
            else
               Put_Line ("Already locked.");
            end if;

         -- Arithmetic operations -
         elsif arg1 = "+" then
            if Integer (VariableStore.Length (ValueStack)) > 2 and VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 2)'Image)) and VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image)) then
               Calculator.Plus (ValueStack);
            else
               Put_Line ("Invalid Stack.");
               return;
            end if;
         elsif arg1 = "-" then
            if Integer (VariableStore.Length (ValueStack)) > 2 and VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 2)'Image)) and VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image)) then
               Calculator.Minus (ValueStack);
            else
               Put_Line ("Invalid Stack.");
               return;
            end if;
         elsif arg1 = "*" then
            if Integer (VariableStore.Length (ValueStack)) > 2 and VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 2)'Image)) and VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image)) then
               Calculator.Multiply (ValueStack);
            else
               Put_Line ("Invalid Stack.");
               return;
            end if;
         elsif arg1 = "/" then
            if Integer (VariableStore.Length (ValueStack)) > 2 and VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 2)'Image)) and VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image))then
               Calculator.Divide (ValueStack);
            else
               Put_Line ("Invalid Stack.");
               return;
            end if;

         -- store operations
         elsif arg1 = "push" then
            if (VariableStore.Length (ValueStack) < 512 or VariableStore.Has_Variable (ValueStack, VariableStore.From_String (Integer (Integer (VariableStore.Length (ValueStack)))'Image))) then
               Calculator.Push(ValueStack, StringToInteger.From_String (arg2));
            else
               Put_Line ("Invalid Operation.");
               return;
            end if;
         elsif arg1 = "pop" then
            if VariableStore.Has_Variable(ValueStack,VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image)) then
               Calculator.Pop (ValueStack);
            else
               Put_Line ("Invalid Operation.");
               return;
            end if;

         elsif arg1 = "load" then
            if (arg2'Length <= VariableStore.Max_Variable_Length and then (VariableStore.Has_Variable(VariableStack, VariableStore.From_String (arg2)) and (VariableStore.Length (ValueStack) < 512 or VariableStore.Has_Variable(ValueStack,VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)))'Image))))) then
               Calculator.Load(ValueStack, VariableStack, VariableStore.From_String (arg2));
            else
               Put_Line ("Invalid Operation.");
               return;
            end if;
         elsif arg1 = "store" then
            if (arg2'Length <= VariableStore.Max_Variable_Length and VariableStore.Has_Variable(ValueStack,VariableStore.From_String(Integer(Integer (VariableStore.Length (ValueStack)) - 1)'Image)))
               and then (VariableStore.Length (VariableStack) < VariableStore.Max_Entries or VariableStore.Has_Variable (VariableStack, VariableStore.From_String (arg2))) then
               Calculator.Store(ValueStack, VariableStack, VariableStore.From_String (arg2));
            else
               Put_Line ("Invalid Operation.");
               return;
            end if;
         elsif arg1 = "remove" then
            if (arg2'Length <= VariableStore.Max_Variable_Length and then VariableStore.Has_Variable (VariableStack, VariableStore.From_String (arg2))) then
               Calculator.Remove(VariableStack, VariableStore.From_String (arg2));
            else
               Put_Line ("Invalid Operation.");
               return;
            end if;
         elsif arg1 = "list" then Calculator.List (VariableStack);
         else
            Put_Line ("Invalid command.");
            return;
         end if;
      end;
   end loop;
end Main;
