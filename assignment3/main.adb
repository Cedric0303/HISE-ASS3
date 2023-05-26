-- Names and ids of you and your partner:
-- Jun Li Chen 1043258, Emmanuel Pinca 1080088

-- We have deefined the following security properties to prove that the
-- implementation is secure:
--
-- 1. Arithmetic operations, load, store, remove and lock are only performed
--    when unlocked.
--    This is proven because the IsLocked boolean (calculator state) is passed
--    into the procedures and checked with SPARK to be false (unlocked) in its
--    precondition.
-- 2. Push and pop are only performed when unlocked.
--    Similar to above, IsLocked passed in and checked in its precondition.
-- 3. Lock is only performed when the calculater is in the unlocked state.
--    Similar to above, IsLocked passed in and checked in its precondition.
-- 4. Unlock is only performed when the calculater is in the locked state.
--    Similar to above, IsLocked passed in and checked in its precondition.
-- 5. Lock updates the master PIN when performed.
--    The postcondition of Lock specifies that when converted to a PIN, the
--    argument string that was passed into the procedure is equivalent to the
--    new output PIN generated by the procedure.
-- 6. Unlock changes the state to unlocked if PINs match, otherwise has no
--    change.
--    The precondition of the Unlock procedure only accepts IsLocked as true
--    and the postcondition specifies that if the PINs match, IsLocked must be
--    false, otherwise IsLocked remains as true.
-- 7. Program starts with a valid PIN.
--    All checks are performed similar to when locking the calculator. The
--    length of the string and each character is checked using IsInvalidPIN,
--    returning true if invalid or false if valid.

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
   IsValidCmd    : Boolean;

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
      if IsLocked then
         Put ("locked> ");
      else
         Put ("unlocked>   ");
      end if;

      Lines.Get_Line(S);
      CommandLineActions.ProcessLine(Lines.To_String(S), IsValidCmd, TokStr1, TokStr2);
      if IsValidCmd then
         declare
            arg1 : String := To_String (TokStr1);
            arg2 : String := To_String (TokStr2);
         begin
            -- Lock operations - no effect when state matches lock state
            if IsLocked then
               if arg1 = "unlock" then
                  if Lock.IsInvalidPIN (arg2) then
                     Put_Line ("Invalid PIN.");
                  else
                     Lock.Unlock (CurrentPIN, arg2, IsLocked);
                  end if;

                  if IsLocked then
                     Put_Line("Incorrect PIN.");
                  end if;

               elsif arg1 = "lock" then
                  Put_Line ("Already locked.");
               end if;

            -- Unlock operations - no effect when state matches unlock state
            elsif not IsLocked then
               if arg1 = "lock" then
                  if Lock.IsInvalidPIN (arg2) then
                     Put_Line ("Invalid PIN.");
                  else
                     Lock.Lock (CurrentPIN, arg2, IsLocked);
                  end if;

               elsif arg1 = "unlock" then
                  Put_Line ("Already unlocked.");

               -- Arithmetic operations -
               elsif arg1 = "+" then
                  if Integer (VariableStore.Length (ValueStack)) >= 2 and 
                    VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 2)'Image)) and 
                    VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image)) 
                  then
                     Calculator.Plus (ValueStack, IsLocked);
                  else
                     Put_Line ("Invalid Stack.");
                     return;
                  end if;
               elsif arg1 = "-" then
                  if Integer (VariableStore.Length (ValueStack)) >= 2 and 
                    VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 2)'Image)) and 
                    VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image)) 
                  then
                     Calculator.Minus (ValueStack, IsLocked);
                  else
                     Put_Line ("Invalid Stack.");
                     return;
                  end if;
               elsif arg1 = "*" then
                  if Integer (VariableStore.Length (ValueStack)) >= 2 and 
                    VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 2)'Image)) and 
                    VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image)) 
                  then
                     Calculator.Multiply (ValueStack, IsLocked);
                  else
                     Put_Line ("Invalid Stack.");
                     return;
                  end if;
               elsif arg1 = "/" then
                  if Integer (VariableStore.Length (ValueStack)) >= 2 and 
                    VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 2)'Image)) and 
                    VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image)) 
                  then
                     Calculator.Divide (ValueStack, IsLocked);
                  else
                     Put_Line ("Invalid Stack.");
                     return;
                  end if;

               -- store operations
               elsif arg1 = "push" then
                  if (VariableStore.Length (ValueStack) < Calculator.Value_Stack_Size or 
                    VariableStore.Has_Variable (ValueStack, VariableStore.From_String (Integer (Integer (VariableStore.Length (ValueStack)))'Image))) 
                  then
                     Calculator.Push(ValueStack, StringToInteger.From_String (arg2), IsLocked);
                  else
                     Put_Line ("Invalid Operation.");
                     return;
                  end if;
               elsif arg1 = "pop" then
                  if VariableStore.Has_Variable(ValueStack,VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image)) 
                  then
                     Calculator.Pop (ValueStack, IsLocked);
                  else
                     Put_Line ("Invalid Operation.");
                     return;
                  end if;

               elsif arg1 = "load" then
                  if (arg2'Length <= VariableStore.Max_Variable_Length and then (VariableStore.Has_Variable(VariableStack, VariableStore.From_String (arg2)) and
                    (VariableStore.Length (ValueStack) < Calculator.Value_Stack_Size or 
                    VariableStore.Has_Variable(ValueStack,VariableStore.From_String(Integer (Integer (VariableStore.Length (ValueStack)))'Image))))) 
                  then
                     Calculator.Load(ValueStack, VariableStack, VariableStore.From_String (arg2), IsLocked);
                  else
                     Put_Line ("Invalid Operation.");
                     return;
                  end if;
               elsif arg1 = "store" then
                  if (arg2'Length <= VariableStore.Max_Variable_Length and 
                    VariableStore.Has_Variable(ValueStack,VariableStore.From_String(Integer(Integer (VariableStore.Length (ValueStack)) - 1)'Image))) and then 
                    (VariableStore.Length (VariableStack) < VariableStore.Max_Entries or 
                    VariableStore.Has_Variable (VariableStack, VariableStore.From_String (arg2))) 
                  then
                     Calculator.Store(ValueStack, VariableStack, VariableStore.From_String (arg2), IsLocked);
                  else
                     Put_Line ("Invalid Operation.");
                     return;
                  end if;
               elsif arg1 = "remove" then
                  if (arg2'Length <= VariableStore.Max_Variable_Length and then 
                    VariableStore.Has_Variable (VariableStack, VariableStore.From_String (arg2))) 
                  then
                     Calculator.Remove(VariableStack, VariableStore.From_String (arg2), IsLocked);
                  else
                     Put_Line ("Invalid Operation.");
                     return;
                  end if;
               elsif arg1 = "list" then 
                  Calculator.List (VariableStack, IsLocked);
               else
                  Put_Line ("Invalid command.");
                  return;
               end if;
            end if;
         end;
      end if;
   end loop;
end Main;
