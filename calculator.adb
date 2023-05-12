with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with VariableStore;
with StringToInteger;
with IntegerToString;

package body Calculator is
   DB : VariableStore.Database;
   CA : CommandArray;
   NumCommands : Natural := 1;
   Increment : Natural := 0;
   
   procedure Init is
   begin
      VariableStore.Init(DB);
   end Init;

   procedure Process(arg1 : in String; arg2 : in String) is
   begin
      --  Put(Integer (VariableStore.Length(db)));Put_Line("");
      --  Put(arg1);Put(":");Put_Line(arg2);
      if arg1 = "+" then
         Plus;
      elsif arg1 = "-" then
         Minus;
      elsif arg1 = "*" then
         Multiply;
      elsif arg1 = "/" then
         Divide;
      elsif arg1 = "push" then
         Push(StringToInteger.From_String(arg2));
      elsif arg1 = "pop" then
         Pop;
      elsif arg1 = "load" then
         Load(VariableStore.From_String(arg2));
      elsif arg1 = "store" then
         Store(VariableStore.From_String(arg2));
      elsif arg1 = "remove" then
         Remove(VariableStore.From_String(arg2));
      elsif arg1 = "list" then
         List;
      else
         Put_Line("Invalid command.");
      end if;
      Put("commands left: ");
      for I in 1..NumCommands - 1 loop
         Put(VariableStore.To_String(CA(I)));
         Put(" ");
      end loop;
      Put_Line("");
   end Process;

   procedure Plus is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(NumCommands - 1);
         lastlastcommand : VariableStore.Variable := CA(NumCommands - 2);
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Increment));
      begin
         NumCommands := NumCommands - 2;

         val1 := VariableStore.Get(DB, lastlastcommand);
         val2 := VariableStore.Get(DB, lastcommand);

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 + val2);

         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;

         Increment := Increment + 1;
      end;
   end Plus;

   procedure Minus is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(NumCommands - 1);
         lastlastcommand : VariableStore.Variable := CA(NumCommands - 2);
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Increment));
      begin
         NumCommands := NumCommands - 2;

         val1 := VariableStore.Get(DB, lastlastcommand);
         val2 := VariableStore.Get(DB, lastcommand);

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 - val2);

         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;

         Increment := Increment + 1;
      end;
   end Minus;

   procedure Multiply is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(NumCommands - 1);
         lastlastcommand : VariableStore.Variable := CA(NumCommands - 2);
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Increment));
      begin
         NumCommands := NumCommands - 2;

         val1 := VariableStore.Get(DB, lastlastcommand);
         val2 := VariableStore.Get(DB, lastcommand);

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 * val2);

         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;

         Increment := Increment + 1;
      end;
   end Multiply;

   procedure Divide is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(NumCommands - 1);
         lastlastcommand : VariableStore.Variable := CA(NumCommands - 2);
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Increment));
      begin
         NumCommands := NumCommands - 2;

         val1 := VariableStore.Get(DB, lastlastcommand);
         val2 := VariableStore.Get(DB, lastcommand);

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 / val2);

         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;

         Increment := Increment + 1;
      end;
   end Divide;

   procedure Push(value : in Integer) is
   begin
      declare
         var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Increment));
      begin
         VariableStore.Put(DB, var, value);

         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;

         Increment := Increment + 1;
      end;
   end Push;
   
   procedure Pop is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(NumCommands - 1);
      begin
         VariableStore.Remove(DB, lastcommand);

         NumCommands := NumCommands - 1;
      end;
   end Pop;

   procedure Load(var : VariableStore.Variable) is
   begin
      declare
         value : Integer := VariableStore.Get(DB, var);
         new_var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Increment));
      begin
         VariableStore.Put(DB, new_var, value);

         CA(NumCommands) := new_var;
         NumCommands := NumCommands + 1;

         Increment := Increment + 1;
      end;
   end Load;
   
   procedure Store(var : in VariableStore.Variable) is
   begin
      declare
         lastcommand : VariableStore.Variable :=  CA(NumCommands - 1);
         value : Integer := VariableStore.Get(db, lastcommand);
      begin
         NumCommands := NumCommands - 1;
         VariableStore.Remove(DB, lastcommand);
         VariableStore.Put(DB, var, value);
         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;
      end;
   end Store;

   procedure Remove(var : in VariableStore.Variable) is
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

   procedure List is
   begin 
      VariableStore.Print(DB);
   end List;

end Calculator;
