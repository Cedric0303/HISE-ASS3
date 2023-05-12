with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded;    use Ada.Strings.Bounded;
with VariableStore;
with StringToInteger;
with IntegerToString;

package body Calculator is
   DB : VariableStore.Database;
   CA : command_record_array;
   num_CA : Natural := 1;
   
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
      end if;
      Put_Line("");
      Put("command num: ");Put_Line(IntegerToString.To_String(num_CA - 1));
   end Process;

   procedure Plus is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(num_CA - 1);
         lastlastcommand : VariableStore.Variable := CA(num_CA - 2);
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String("plus" & IntegerToString.To_String(Integer(VariableStore.Length(DB))));
      begin
         num_CA := num_CA - 2;

         val1 := VariableStore.Get(DB, lastlastcommand);
         val2 := VariableStore.Get(DB, lastcommand);

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 + val2);

         CA(num_CA) := var;
         num_CA := num_CA + 1;
      end;
   end Plus;

   procedure Minus is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(num_CA - 1);
         lastlastcommand : VariableStore.Variable := CA(num_CA - 2);
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String("mins" & IntegerToString.To_String(Integer(VariableStore.Length(DB))));
      begin
         num_CA := num_CA - 2;

         val1 := VariableStore.Get(DB, lastlastcommand);
         val2 := VariableStore.Get(DB, lastcommand);

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 - val2);

         CA(num_CA) := var;
         num_CA := num_CA + 1;
      end;
   end Minus;

   procedure Multiply is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(num_CA - 1);
         lastlastcommand : VariableStore.Variable := CA(num_CA - 2);
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String("mult" & IntegerToString.To_String(Integer(VariableStore.Length(DB))));
      begin
         num_CA := num_CA - 2;

         val1 := VariableStore.Get(DB, lastlastcommand);
         val2 := VariableStore.Get(DB, lastcommand);

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 * val2);

         CA(num_CA) := var;
         num_CA := num_CA + 1;
      end;
   end Multiply;

   procedure Divide is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(num_CA - 1);
         lastlastcommand : VariableStore.Variable := CA(num_CA - 2);
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String("divd" & IntegerToString.To_String(Integer(VariableStore.Length(DB))));
      begin
         num_CA := num_CA - 2;

         val1 := VariableStore.Get(DB, lastlastcommand);
         val2 := VariableStore.Get(DB, lastcommand);

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 / val2);

         CA(num_CA) := var;
         num_CA := num_CA + 1;
      end;
   end Divide;

   procedure Push(value : in Integer) is
   begin
      declare
         var : VariableStore.Variable := VariableStore.From_String("push" & IntegerToString.To_String(Integer(VariableStore.Length(DB))));
      begin
         VariableStore.Put(DB, var, value);
         CA(num_CA) := var;
         num_CA := num_CA + 1;
      end;
   end Push;
   
   procedure Pop is
   begin
      declare
         lastcommand : VariableStore.Variable := CA(num_CA - 1);
      begin
         VariableStore.Remove(DB, lastcommand);
         num_CA := num_CA - 1;
      end;
   end Pop;

   procedure Load(var : VariableStore.Variable) is
   begin
      declare
         value : Integer := VariableStore.Get(DB, var);
         new_var : VariableStore.Variable := VariableStore.From_String("load" & IntegerToString.To_String(Integer(VariableStore.Length(DB))));
      begin
         VariableStore.Put(DB, new_var, value);
         CA(num_CA) := new_var;
         num_CA := num_CA + 1;
      end;
   end Load;
   
   procedure Store(var : in VariableStore.Variable) is
   begin
      declare
         lastcommand : VariableStore.Variable :=  CA(num_CA - 1);
         value : Integer := VariableStore.Get(db, lastcommand);
      begin
         num_CA := num_CA - 1;
         VariableStore.Remove(DB, lastcommand);
         VariableStore.Put(DB, var, value);
         CA(num_CA) := var;
         num_CA := num_CA + 1;
      end;
   end Store;

   procedure Remove(var : in VariableStore.Variable) is
   begin
      declare
         found : Boolean := false;
      begin
         VariableStore.Remove(DB, var);
         for I in 1..num_CA - 1 loop
            if not found and VariableStore.Equal(CA(I), var) then
               found := True;
            end if;
            if found and I < num_CA - 2 then
               CA(I) := CA(I + 1);
            end if;
         end loop;
         num_CA := num_CA - 1;
      end;
   end Remove;

   procedure List is
   begin 
      VariableStore.Print(DB);
   end List;

end Calculator;
