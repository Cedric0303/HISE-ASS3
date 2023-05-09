with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with VariableStore;
with StringToInteger;
with IntegerToString;

package body Calculator is
   UNLOCKED : Boolean := false;
   DONE : Boolean := false;
   --  commandcount : Positive := 1;

   procedure Process(db : in out VariableStore.Database;
                     commandarray : in out VariableArray;
                     --  commandcount : out Positive;
                     arg1 : in String;
                     arg2 : in String;
                     done : in out Boolean) is
   begin
      --  Put(Integer (VariableStore.Length(db)));Put_Line("");
      --  Put(arg1);Put(":");Put_Line(arg2);
      if arg1 = "push" then
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
      for I in 1..Integer(Length(commandarray)) loop
   --          ^ Gets the range of Tab
      Put_Line(VariableStore.To_String(commandarray (I)));
   end loop;
   end Process;

   procedure Push(db : in out VariableStore.Database;
                  commandarray : in out VariableArray;
                  value : in Integer) is
   begin
      declare
         var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Integer(VariableStore.Length(db))) & "push");
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
         var : VariableStore.Variable := VariableStore.From_String(IntegerToString.To_String(Integer(VariableStore.Length(db))) & "load");
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
