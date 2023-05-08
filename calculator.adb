with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with VariableStore;
with StringToInteger;

--  pragma Ada_2012;
package body Calculator is
   UNLOCKED : Boolean := false;
   DONE : Boolean := false;

   procedure Process(db : in out VariableStore.Database;
                     lastcommand : in out VariableStore.Variable;
                     arg1 : in String;
                     arg2 : in String;
                     done : in out Boolean) is
   begin
      --  Put(Integer (VariableStore.Length(db)));Put_Line("");
      --  Put(arg1);Put(":");Put_Line(arg2);
      if arg1 = "push" then
         Push(db, VariableStore.From_String(arg1), StringToInteger.From_String(arg2));
         lastcommand := VariableStore.From_String((Integer'Image(Integer(VariableStore.Length(db)))));
      elsif arg1 = "list" then
         List(db);
      elsif arg1 = "pop" then
         Pop(db, lastcommand);
      elsif arg1 = "exit" then
         done := True;
      end if;
   end Process;

   procedure Push(db : in out VariableStore.Database;
                  var : in VariableStore.Variable; 
                  value : in Integer) is
   begin
       VariableStore.Put(db, VariableStore.From_String((Integer'Image(Integer(VariableStore.Length(db)) + 1))), value);
   end Push;
   
   procedure Pop(db : in out VariableStore.Database;
                 lastcommand : in VariableStore.Variable) is
   begin
       VariableStore.Remove(db, lastcommand);
   end Pop;

   procedure List(db : in VariableStore.Database) is
   begin 
      VariableStore.Print(db);
   end List;

end Calculator;
