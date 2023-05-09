with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with VariableStore;
with StringToInteger;
with IntegerToString;

package body Calculator is
   UNLOCKED : Boolean := false;
   DONE : Boolean := false;

   procedure Process(db : in out VariableStore.Database;
                     commandarray : in out VariableArray;
                     arg1 : in String;
                     arg2 : in String;
                     done : in out Boolean) is
   begin
      --  Put(Integer (VariableStore.Length(db)));Put_Line("");
      --  Put(arg1);Put(":");Put_Line(arg2);
      if arg1 = "+" then
         Plus(db, commandarray);
      elsif arg1 = "-" then
         Minus(db, commandarray);
      elsif arg1 = "*" then
         Multiply(db, commandarray);
      elsif arg1 = "/" then
         Divide(db, commandarray);
      elsif arg1 = "push" then
         Push(db, commandarray, StringToInteger.From_String(arg2));
      elsif arg1 = "pop" then
         Pop(db, commandarray);
      elsif arg1 = "load" then
         Load(db, commandarray, VariableStore.From_String(arg2));
      elsif arg1 = "store" then
         Store(db, commandarray, VariableStore.From_String(arg2));
      elsif arg1 = "remove" then
         Remove(db, commandarray, VariableStore.From_String(arg2));
      elsif arg1 = "list" then
         List(db);
      elsif arg1 = "exit" then
         done := True;
      end if;
   end Process;

   procedure Plus(db : in out VariableStore.Database;
                  commandarray : in out VariableArray) is
   begin
      declare
         lastcommand : VariableStore.Variable;
         lastlastcommand : VariableStore.Variable;
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String("plus" & IntegerToString.To_String(Integer(VariableStore.Length(db))));
      begin
         lastcommand := Last_Element(commandarray);
         Delete_Last(commandarray);
         lastlastcommand := Last_Element(commandarray);
         Delete_Last(commandarray);

         val1 := VariableStore.Get(db, lastlastcommand);
         val2 := VariableStore.Get(db, lastcommand);

         VariableStore.Remove(db, lastcommand);
         VariableStore.Remove(db, lastlastcommand);

         VariableStore.Put(db, var, val1 + val2);

         Append(commandarray, var);
      end;
   end Plus;

   procedure Minus(db : in out VariableStore.Database;
                   commandarray : in out VariableArray) is
   begin
      declare
         lastcommand : VariableStore.Variable;
         lastlastcommand : VariableStore.Variable;
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String("mins" & IntegerToString.To_String(Integer(VariableStore.Length(db))));
      begin
         lastcommand := Last_Element(commandarray);
         Delete_Last(commandarray);
         lastlastcommand := Last_Element(commandarray);
         Delete_Last(commandarray);

         val1 := VariableStore.Get(db, lastlastcommand);
         val2 := VariableStore.Get(db, lastcommand);

         VariableStore.Remove(db, lastcommand);
         VariableStore.Remove(db, lastlastcommand);

         VariableStore.Put(db, var, val1 - val2);

         Append(commandarray, var);
      end;
   end Minus;

   procedure Multiply(db : in out VariableStore.Database;
                      commandarray : in out VariableArray) is
   begin
      declare
         lastcommand : VariableStore.Variable;
         lastlastcommand : VariableStore.Variable;
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String("mult" & IntegerToString.To_String(Integer(VariableStore.Length(db))));
      begin
         lastcommand := Last_Element(commandarray);
         Delete_Last(commandarray);
         lastlastcommand := Last_Element(commandarray);
         Delete_Last(commandarray);

         val1 := VariableStore.Get(db, lastlastcommand);
         val2 := VariableStore.Get(db, lastcommand);

         VariableStore.Remove(db, lastcommand);
         VariableStore.Remove(db, lastlastcommand);

         VariableStore.Put(db, var, val1 * val2);

         Append(commandarray, var);
      end;
   end Multiply;

   procedure Divide(db : in out VariableStore.Database;
                    commandarray : in out VariableArray) is
   begin
      declare
         lastcommand : VariableStore.Variable;
         lastlastcommand : VariableStore.Variable;
         val1 : Integer;
         val2 : Integer;
         var : VariableStore.Variable := VariableStore.From_String("divd" & IntegerToString.To_String(Integer(VariableStore.Length(db))));
      begin
         lastcommand := Last_Element(commandarray);
         Delete_Last(commandarray);
         lastlastcommand := Last_Element(commandarray);
         Delete_Last(commandarray);

         val1 := VariableStore.Get(db, lastlastcommand);
         val2 := VariableStore.Get(db, lastcommand);

         VariableStore.Remove(db, lastcommand);
         VariableStore.Remove(db, lastlastcommand);

         VariableStore.Put(db, var, val1 / val2);

         Append(commandarray, var);
      end;
   end Divide;

   procedure Push(db : in out VariableStore.Database;
                  commandarray : in out VariableArray;
                  value : in Integer) is
   begin
      declare
         var : VariableStore.Variable := VariableStore.From_String("push" & IntegerToString.To_String(Integer(VariableStore.Length(db))));
      begin
         VariableStore.Put(db, var, value);
         Append(commandarray,var);
      end;
   end Push;
   
   procedure Pop(db : in out VariableStore.Database;
                 commandarray : in out VariableArray) is
   begin
      declare
         lastcommand : VariableStore.Variable := Last_Element(commandarray);
      begin
         VariableStore.Remove(db, lastcommand);
         Delete(commandarray, Last_Index(commandarray));
      end;
   end Pop;

   procedure Load(db : in out VariableStore.Database;
                  commandarray : in out VariableArray;
                  var : VariableStore.Variable) is
   begin
      declare
         lastcommand : VariableStore.Variable := Last_Element(commandarray);
         value : Integer := VariableStore.Get(db, lastcommand);
         var : VariableStore.Variable := VariableStore.From_String("load" & IntegerToString.To_String(Integer(VariableStore.Length(db))));
      begin
         VariableStore.Put(db, var, value);
         Append(commandarray, var);
      end;
   end Load;
   
   procedure Store(db : in out VariableStore.Database;
                   commandarray : in out VariableArray;
                   var : in VariableStore.Variable) is
   begin
      declare
         lastcommand : VariableStore.Variable := Last_Element(commandarray);
         value : Integer := VariableStore.Get(db, lastcommand);
      begin
         VariableStore.Remove(db, lastcommand);
         VariableStore.Put(db, var, value);
         Delete_Last(commandarray);
         Append(commandarray, var);
      end;
   end Store;

   procedure Remove(db : in out VariableStore.Database;
                    commandarray : in out VariableArray;
                    var : in VariableStore.Variable) is
   begin
      VariableStore.Remove(db, var);
      Delete(commandarray, commandarray.Find_Index(var));
   end Remove;

   procedure List(db : in VariableStore.Database) is
   begin 
      VariableStore.Print(db);
   end List;

end Calculator;
