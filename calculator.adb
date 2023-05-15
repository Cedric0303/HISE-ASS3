with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers; use Ada.Containers;
with VariableStore;
with StringToInteger;
with IntegerToString;

package body Calculator with SPARK_Mode is
   
   procedure Plus(DB : in out VariableStore.Database;
                  CA : in out CommandArray) is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(NumCommands - 1);
         lastlastcommand : VariableStore.Variable := CA(NumCommands - 2);
         val1 : Integer := VariableStore.Get(DB, lastlastcommand);
         val2 : Integer := VariableStore.Get(DB, lastcommand);
         var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Increment));
      begin
         NumCommands := NumCommands - 2;
         if VariableStore.Has_Variable(DB, lastcommand) then
            VariableStore.Remove(DB, lastcommand);
         end if;
         if VariableStore.Has_Variable(DB, lastlastcommand) then
            VariableStore.Remove(DB, lastlastcommand);
         end if;

         if VariableStore.Length(DB) < VariableStore.Max_Entries or 
           VariableStore.Has_Variable(DB, var) then
            if ((val1 >= 0 and then val2 <= Integer'Last - val1) or else 
                  (val1 < 0 and then val2 >= Integer'First - val1))  then
               VariableStore.Put(DB, var, val1 + val2);
            end if;
         end if;
         
         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;

         Increment := Increment + 1;
      end;
   end Plus;
  
   procedure Minus(DB : in out VariableStore.Database;
                  CA : in out CommandArray) is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(NumCommands - 1);
         lastlastcommand : VariableStore.Variable := CA(NumCommands - 2);
         val1 : Integer := VariableStore.Get(DB, lastlastcommand);
         val2 : Integer := VariableStore.Get(DB, lastcommand);
         var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Increment));
      begin
         NumCommands := NumCommands - 2;
         if VariableStore.Has_Variable(DB, lastcommand) then
            VariableStore.Remove(DB, lastcommand);
         end if;
         if VariableStore.Has_Variable(DB, lastlastcommand) then
            VariableStore.Remove(DB, lastlastcommand);
         end if;

         if VariableStore.Length(DB) < VariableStore.Max_Entries or 
           VariableStore.Has_Variable(DB, var) then
            if ((val1 >= 0 and then val2 >= Integer'Last - val1) or else 
                  (val1 < 0 and then val2 <= Integer'First - val1))  then
               VariableStore.Put(DB, var, val1 - val2);
            end if;
         end if;
         
         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;

         Increment := Increment + 1;
      end;
   end Minus;
   
   procedure Multiply(DB : in out VariableStore.Database;
                      CA : in out CommandArray) is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(NumCommands - 1);
         lastlastcommand : VariableStore.Variable := CA(NumCommands - 2);
         val1 : Integer := VariableStore.Get(DB, lastlastcommand);
         val2 : Integer := VariableStore.Get(DB, lastcommand);
         var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Increment));
      begin
         NumCommands := NumCommands - 2;
         if VariableStore.Has_Variable(DB, lastcommand) then
            VariableStore.Remove(DB, lastcommand);
         end if;
         if VariableStore.Has_Variable(DB, lastlastcommand) then
            VariableStore.Remove(DB, lastlastcommand);
         end if;

         if VariableStore.Length(DB) < VariableStore.Max_Entries or 
           VariableStore.Has_Variable(DB, var) then
            if (((val1 < 2**15 and val2 < ((2**16) - 1)) and (val1 > -2**15 and val2 > -2**16)) or else 
                  ((val1 < ((2**16) - 1) and val2 < 2**15) and (val1 > -2**16 and val2 > -2**15))) then
               VariableStore.Put(DB, var, val1 * val2);
            end if;
         end if;
         
         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;

         Increment := Increment + 1;
      end;
   end Multiply;
   
   procedure Divide(DB : in out VariableStore.Database;
                    CA : in out CommandArray) is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(NumCommands - 1);
         lastlastcommand : VariableStore.Variable := CA(NumCommands - 2);
         val1 : Integer := VariableStore.Get(DB, lastlastcommand);
         val2 : Integer := VariableStore.Get(DB, lastcommand);
         var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Increment));
      begin
         NumCommands := NumCommands - 2;
         if VariableStore.Has_Variable(DB, lastcommand) then
            VariableStore.Remove(DB, lastcommand);
         end if;
         if VariableStore.Has_Variable(DB, lastlastcommand) then
            VariableStore.Remove(DB, lastlastcommand);
         end if;

         if VariableStore.Length(DB) < VariableStore.Max_Entries or 
           VariableStore.Has_Variable(DB, var) then
            if (val2 /= 0 and not (val1 = Integer'First and val2 = -1)) then
               VariableStore.Put(DB, var, val1 / val2);
            end if;
         end if;
         
         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;

         Increment := Increment + 1;
      end;
   end Divide;
   
   procedure Push(DB : in out VariableStore.Database;
                  CA : in out CommandArray;
                  value : in Integer) is
   begin
      declare
         var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Increment));
      begin
         if VariableStore.Length(DB) < VariableStore.Max_Entries or 
           VariableStore.Has_Variable(DB, var) then
            VariableStore.Put(DB, var, value);
         end if;
            
         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;
   
         Increment := Increment + 1;
      end;
   end Push;
   
   procedure Pop(DB : in out VariableStore.Database;
                 CA : in CommandArray) is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(NumCommands - 1);
      begin
         VariableStore.Remove(DB, lastcommand);
   
         NumCommands := NumCommands - 1;
      end;
   end Pop;
   
   procedure Load(DB : in out VariableStore.Database;
                  CA : in out CommandArray;
                  old_var : VariableStore.Variable) is
   begin
      declare
         value : Integer := VariableStore.Get(DB, old_var);
         new_var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Increment));
      begin
         if VariableStore.Length(DB) < VariableStore.Max_Entries or 
           VariableStore.Has_Variable(DB, new_var) then
            VariableStore.Put(DB, new_var, value);
         end if;
   
         CA(NumCommands) := new_var;
         NumCommands := NumCommands + 1;
   
         Increment := Increment + 1;
      end;
   end Load;
   
   procedure Store(DB : in out VariableStore.Database;
                   CA : in out CommandArray;
                   var : in VariableStore.Variable) is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(NumCommands - 1);
         value : Integer := VariableStore.Get(DB, lastcommand);
      begin
         NumCommands := NumCommands - 1;
   
         VariableStore.Remove(DB, lastcommand);
         if VariableStore.Length(DB) < VariableStore.Max_Entries or 
           VariableStore.Has_Variable(DB, var) then
            VariableStore.Put(DB, var, value);
         end if;
   
         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;
      end;
   end Store;
   
   procedure Remove(DB : in out VariableStore.Database;
                    CA : in out CommandArray;
                    var : in VariableStore.Variable) is
   begin
      declare
         found : Boolean := false;
      begin
         VariableStore.Remove(DB, var);
         for I in 1..NumCommands - 1 loop
            if not found and VariableStore.Equal(CA(I), var) then
               found := True;
            end if;
            if found and I < NumCommands - 1 then
               CA(I) := CA(I + 1);
            end if;
         end loop;
         NumCommands := NumCommands - 1;
      end;
   end Remove;

   procedure List(DB : in VariableStore.Database) is
   begin 
      VariableStore.Print(DB);
   end List;

end Calculator;
