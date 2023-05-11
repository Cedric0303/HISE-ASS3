with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with VariableStore;
with StringToInteger;
with IntegerToString;

package body Calculator is
   DB : VariableStore.Database;
   CommandArray : Calculator.VariableArray;
   
   procedure Init is
   begin
      VariableStore.Init(DB);
   end Init;

   procedure Process(arg1 : in String;
                     arg2 : in String) is
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
   end Process;

   procedure Plus is
   begin
      declare
         lastcommand : VariableStore.Variable;
         lastlastcommand : VariableStore.Variable;
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String("plus" & IntegerToString.To_String(Integer(VariableStore.Length(DB))));
      begin
         lastcommand := Last_Element(CommandArray);
         Delete_Last(CommandArray);
         lastlastcommand := Last_Element(CommandArray);
         Delete_Last(CommandArray);

         val1 := VariableStore.Get(DB, lastlastcommand);
         val2 := VariableStore.Get(DB, lastcommand);

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 + val2);

         Append(CommandArray, var);
      end;
   end Plus;

   procedure Minus is
   begin
      declare
         lastcommand : VariableStore.Variable;
         lastlastcommand : VariableStore.Variable;
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String("mins" & IntegerToString.To_String(Integer(VariableStore.Length(DB))));
      begin
         lastcommand := Last_Element(CommandArray);
         Delete_Last(CommandArray);
         lastlastcommand := Last_Element(CommandArray);
         Delete_Last(CommandArray);

         val1 := VariableStore.Get(DB, lastlastcommand);
         val2 := VariableStore.Get(DB, lastcommand);

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 - val2);

         Append(CommandArray, var);
      end;
   end Minus;

   procedure Multiply is
   begin
      declare
         lastcommand : VariableStore.Variable;
         lastlastcommand : VariableStore.Variable;
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String("mult" & IntegerToString.To_String(Integer(VariableStore.Length(DB))));
      begin
         lastcommand := Last_Element(CommandArray);
         Delete_Last(CommandArray);
         lastlastcommand := Last_Element(CommandArray);
         Delete_Last(CommandArray);

         val1 := VariableStore.Get(DB, lastlastcommand);
         val2 := VariableStore.Get(DB, lastcommand);

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 * val2);

         Append(CommandArray, var);
      end;
   end Multiply;

   procedure Divide is
   begin
      declare
         lastcommand : VariableStore.Variable;
         lastlastcommand : VariableStore.Variable;
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String("divd" & IntegerToString.To_String(Integer(VariableStore.Length(DB))));
      begin
         lastcommand := Last_Element(CommandArray);
         Delete_Last(CommandArray);
         lastlastcommand := Last_Element(CommandArray);
         Delete_Last(CommandArray);

         val1 := VariableStore.Get(DB, lastlastcommand);
         val2 := VariableStore.Get(DB, lastcommand);

         VariableStore.Remove(DB, lastcommand);
         VariableStore.Remove(DB, lastlastcommand);

         VariableStore.Put(DB, var, val1 / val2);

         Append(CommandArray, var);
      end;
   end Divide;

   procedure Push(value : in Integer) is
   begin
      declare
         var : VariableStore.Variable := VariableStore.From_String("push" & IntegerToString.To_String(Integer(VariableStore.Length(DB))));
      begin
         VariableStore.Put(DB, var, value);
         Append(CommandArray, var);
      end;
   end Push;
   
   procedure Pop is
   begin
      declare
         lastcommand : VariableStore.Variable := Last_Element(CommandArray);
      begin
         VariableStore.Remove(DB, lastcommand);
         Delete(CommandArray, Last_Index(CommandArray));
      end;
   end Pop;

   procedure Load(var : VariableStore.Variable) is
   begin
      declare
         lastcommand : VariableStore.Variable := Last_Element(CommandArray);
         value : Integer := VariableStore.Get(DB, lastcommand);
         var : VariableStore.Variable := VariableStore.From_String("load" & IntegerToString.To_String(Integer(VariableStore.Length(DB))));
      begin
         VariableStore.Put(DB, var, value);
         Append(CommandArray, var);
      end;
   end Load;
   
   procedure Store(var : in VariableStore.Variable) is
   begin
      declare
         lastcommand : VariableStore.Variable := Last_Element(CommandArray);
         value : Integer := VariableStore.Get(db, lastcommand);
      begin
         VariableStore.Remove(DB, lastcommand);
         VariableStore.Put(DB, var, value);
         Delete_Last(CommandArray);
         Append(CommandArray, var);
      end;
   end Store;

   procedure Remove(var : in VariableStore.Variable) is
   begin
      VariableStore.Remove(DB, var);
      Delete(CommandArray, CommandArray.Find_Index(var));
   end Remove;

   procedure List is
   begin 
      VariableStore.Print(DB);
   end List;

end Calculator;
