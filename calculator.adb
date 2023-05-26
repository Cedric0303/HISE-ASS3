with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers; use Ada.Containers;
with VariableStore;
with StringToInteger;

package body Calculator with SPARK_Mode is
    
   procedure Plus(ValueStack : in out VariableStore.Database; IsLocked : in Boolean) is
   begin
      declare
         val1var : VariableStore.Variable;
         val2var : VariableStore.Variable;
         stackpos1 : Integer := Integer(VariableStore.Length(ValueStack)) - 2;
         stackpos2 : Integer := Integer(VariableStore.Length(ValueStack)) - 1;
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable;
      begin
         val1var := VariableStore.From_String(stackpos1'Image);
         val2var := VariableStore.From_String(stackpos2'Image);
         var := VariableStore.From_String(stackpos1'Image);

         val1 := VariableStore.Get(ValueStack, val1var);
         val2 := VariableStore.Get(ValueStack, val2var);
         if VariableStore.Has_Variable(ValueStack, val2var) then
            VariableStore.Remove(ValueStack, val2var);
         end if;
         if VariableStore.Has_Variable(ValueStack, val1var) then
            VariableStore.Remove(ValueStack, val1var);
         end if;
         
         if VariableStore.Length(ValueStack) < VariableStore.Max_Entries or
         VariableStore.Has_Variable(ValueStack, var) then
            if ((val1 >= 0 and then val2 <= Integer'Last - val1) or else
            (val1 < 0 and then val2 >= Integer'First - val1))  then
               VariableStore.Put(ValueStack, var, val1 + val2);
            end if;
         end if;
      end;
   end Plus;
  
   procedure Minus(ValueStack : in out VariableStore.Database; IsLocked : in Boolean) is
   begin
      declare
         val1var : VariableStore.Variable;
         val2var : VariableStore.Variable;
         stackpos1 : Integer := Integer(VariableStore.Length(ValueStack)) - 2;
         stackpos2 : Integer := Integer(VariableStore.Length(ValueStack)) - 1;
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable;
      begin
         val1var := VariableStore.From_String(stackpos1'Image);
         val2var := VariableStore.From_String(stackpos2'Image);
         var := VariableStore.From_String(stackpos1'Image);

         val1 := VariableStore.Get(ValueStack, val1var);
         val2 := VariableStore.Get(ValueStack, val2var);
         if VariableStore.Has_Variable(ValueStack, val2var) then
            VariableStore.Remove(ValueStack, val2var);
         end if;
         if VariableStore.Has_Variable(ValueStack, val1var) then
            VariableStore.Remove(ValueStack, val1var);
         end if;
         
         if VariableStore.Length(ValueStack) < VariableStore.Max_Entries or
          VariableStore.Has_Variable(ValueStack, var) then
            if ((val1 >= 0 and then val2 >= Integer'Last - val1) or else
              (val1 < 0 and then val2 <= Integer'First - val1))  then
               VariableStore.Put(ValueStack, var, val1 - val2);
            end if;
         end if;
      end;
   end Minus;
   
   procedure Multiply(ValueStack : in out VariableStore.Database; IsLocked : in Boolean) is
   begin
      declare
         val1var : VariableStore.Variable;
         val2var : VariableStore.Variable;
         stackpos1 : Integer := Integer(VariableStore.Length(ValueStack)) - 2;
         stackpos2 : Integer := Integer(VariableStore.Length(ValueStack)) - 1;
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable;
      begin
         val1var := VariableStore.From_String(stackpos1'Image);
         val2var := VariableStore.From_String(stackpos2'Image);
         var := VariableStore.From_String(stackpos1'Image);

         val1 := VariableStore.Get(ValueStack, val1var);
         val2 := VariableStore.Get(ValueStack, val2var);
         if VariableStore.Has_Variable(ValueStack, val2var) then
            VariableStore.Remove(ValueStack, val2var);
         end if;
         if VariableStore.Has_Variable(ValueStack, val1var) then
            VariableStore.Remove(ValueStack, val1var);
         end if;
         
         if VariableStore.Length(ValueStack) < VariableStore.Max_Entries or
          VariableStore.Has_Variable(ValueStack, var) then
            if (((val1 < 2**15 and val2 < ((2**16) - 1)) and (val1 > -2**15 and val2 > -2**16)) or else
            ((val1 < ((2**16) - 1) and val2 < 2**15) and (val1 > -2**16 and val2 > -2**15))) then
               VariableStore.Put(ValueStack, var, val1 * val2);
            end if;
         end if;
      end;
   end Multiply;
   
   procedure Divide(ValueStack : in out VariableStore.Database; IsLocked : in Boolean) is
   begin
      declare
         val1var : VariableStore.Variable;
         val2var : VariableStore.Variable;
         stackpos1 : Integer := Integer(VariableStore.Length(ValueStack)) - 2;
         stackpos2 : Integer := Integer(VariableStore.Length(ValueStack)) - 1;
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable;
      begin
         val1var := VariableStore.From_String(stackpos1'Image);
         val2var := VariableStore.From_String(stackpos2'Image);
         var := VariableStore.From_String(stackpos1'Image);

         val1 := VariableStore.Get(ValueStack, val1var);
         val2 := VariableStore.Get(ValueStack, val2var);
         if VariableStore.Has_Variable(ValueStack, val2var) then
            VariableStore.Remove(ValueStack, val2var);
         end if;
         if VariableStore.Has_Variable(ValueStack, val1var) then
            VariableStore.Remove(ValueStack, val1var);
         end if;

         if VariableStore.Length(ValueStack) < VariableStore.Max_Entries or
          VariableStore.Has_Variable(ValueStack, var) then
            if (val2 /= 0 and not (val1 = Integer'First and val2 = -1)) then
               VariableStore.Put(ValueStack, var, val1 / val2);
            end if;
         end if;
      end;
   end Divide;
   
   procedure Push(ValueStack : in out VariableStore.Database;
                  value : in Integer; IsLocked : in Boolean) is
   begin
      declare
         length : Integer := Integer(VariableStore.Length(ValueStack));
         var : VariableStore.Variable := VariableStore.From_String(length'Image);
      begin
         VariableStore.Put(ValueStack, var, value);
         --  if VariableStore.Length(ValueStack) < VariableStore.Max_Entries or
         --    VariableStore.Has_Variable(ValueStack, var) then
         --     VariableStore.Put(ValueStack, var, value);
         --  end if;
      end;
   end Push;
   
   procedure Pop(ValueStack : in out VariableStore.Database; IsLocked : in Boolean) is
   begin
      declare
         length : Integer := Integer(VariableStore.Length(ValueStack)) - 1;
         lastcommand : VariableStore.Variable := VariableStore.From_String(length'Image);
      begin
         VariableStore.Remove(ValueStack, lastcommand);
      end;
   end Pop;
   
   procedure Load(ValueStack : in out VariableStore.Database;
                  VariableStack : in VariableStore.Database;
                  OldVar : in VariableStore.Variable; IsLocked : in Boolean) is
   begin
      declare
         value : Integer;
         length : Integer := Integer(VariableStore.Length(ValueStack));
         NewVar : VariableStore.Variable := VariableStore.From_String(length'Image);
      begin        
         value := VariableStore.Get(VariableStack, OldVar);
         VariableStore.Put(ValueStack, NewVar, value);
      end;
   end Load;
   
   procedure Store(ValueStack : in out VariableStore.Database;
                   VariableStack : in out VariableStore.Database;
                   var : in VariableStore.Variable; IsLocked : in Boolean) is
   begin
      declare
         length : Integer := Integer(VariableStore.Length(ValueStack)) - 1;
         lastcommand : VariableStore.Variable;
         value : Integer;
      begin
         lastcommand := VariableStore.From_String(length'Image);

         value := VariableStore.Get(ValueStack, lastcommand);
         VariableStore.Remove(ValueStack, lastcommand);
         VariableStore.Put(VariableStack, var, value);
      end;
   end Store;
   
   procedure Remove(VariableStack : in out VariableStore.Database;
                    var : in VariableStore.Variable; IsLocked : in Boolean) is
   begin
      VariableStore.Remove(VariableStack, var);
   end Remove;

end Calculator;
