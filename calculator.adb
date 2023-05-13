with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with VariableStore;
with StringToInteger;
with IntegerToString;

package body Calculator with SPARK_Mode is
   
   procedure Process(DB : in out VariableStore.Database;
                     CA : in out CommandArray;
                     arg1 : in String; 
                     arg2 : in String) is
   begin
      --  Put(Integer (VariableStore.Length(DB)));Put_Line("");
      --  Put(arg1);Put(":");Put_Line(arg2);
      if arg1 = "+" then
         Plus(DB, CA);
      elsif arg1 = "-" then
         Minus(DB, CA);
      elsif arg1 = "*" then
         Multiply(DB, CA);
      elsif arg1 = "/" then
         Divide(DB, CA);
      elsif arg1 = "push" then
         Push(DB, CA, StringToInteger.From_String(arg2));
      elsif arg1 = "pop" then
         Pop(DB, CA);
      elsif arg1 = "load" then
         Load(DB, CA, VariableStore.From_String(arg2));
      elsif arg1 = "store" then
         Store(DB, CA, VariableStore.From_String(arg2));
      elsif arg1 = "remove" then
         Remove(DB, CA, VariableStore.From_String(arg2));
      elsif arg1 = "list" then
         List(DB);
      else
         Put_Line("Invalid command.");
      end if;
      Put("commands left: [");
      for I in 1..NumCommands - 1 loop
         Put(VariableStore.To_String(CA(I)));
         Put(" ");
      end loop;
      Put_Line("]");
   end Process;

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

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 + val2);

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

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 - val2);

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

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 * val2);

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

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 / val2);

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
         VariableStore.Put(DB, var, value);

         CA(NumCommands) := var;
         NumCommands := NumCommands + 1;

         Increment := Increment + 1;
      end;
   end Push;
   
   procedure Pop(DB : in out VariableStore.Database;
                 CA : in out CommandArray) is
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
                  var : VariableStore.Variable) is
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
         VariableStore.Put(DB, var, value);

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

   procedure List(DB : in out VariableStore.Database) is
   begin 
      VariableStore.Print(DB);
   end List;

end Calculator;
