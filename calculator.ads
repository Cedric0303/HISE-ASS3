with MyStringTokeniser;
with VariableStore;
with Ada.Containers; use Ada.Containers;

package Calculator with SPARK_Mode is 
     
   procedure Plus(ValueStack : in out VariableStore.Database) with
     Pre =>
       Integer(VariableStore.Length(ValueStack)) > 2 and
       VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 2)'Image)) and
       VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 1)'Image));
         
   procedure Minus(ValueStack : in out VariableStore.Database) with
     Pre =>
       Integer(VariableStore.Length(ValueStack)) > 2 and
       VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 2)'Image)) and
       VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 1)'Image));

   procedure Multiply(ValueStack : in out VariableStore.Database) with
     Pre =>
       Integer(VariableStore.Length(ValueStack)) > 2 and
       VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 2)'Image)) and
       VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 1)'Image));
   
   procedure Divide(ValueStack : in out VariableStore.Database) with
     Pre =>
       Integer(VariableStore.Length(ValueStack)) > 2 and
       VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 2)'Image)) and
       VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 1)'Image));
   
   procedure Push(ValueStack : in out VariableStore.Database;
                  value : in Integer) with
     Pre =>
       VariableStore.Length(ValueStack) < VariableStore.Max_Entries or 
       VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)))'Image));
   
   procedure Pop(ValueStack : in out VariableStore.Database) with 
     Pre =>
       VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 1)'Image));
       
   procedure Load(ValueStack : in out VariableStore.Database;
                  VariableStack : in VariableStore.Database;
                  OldVar : in VariableStore.Variable) with
     Pre =>
       VariableStore.Has_Variable(VariableStack, OldVar) and
       (VariableStore.Length(ValueStack) < VariableStore.Max_Entries or 
       VariableStore.Has_Variable(ValueStack, VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)))'Image)));
       
   
   procedure Store(ValueStack : in out VariableStore.Database;
                   VariableStack : in out VariableStore.Database;
                   var : in VariableStore.Variable) with
     Pre => 
       VariableStore.Has_Variable(ValueStack, 
                                  VariableStore.From_String(Integer(Integer(VariableStore.Length(ValueStack)) - 1)'Image)) and
       (VariableStore.Length(VariableStack) < VariableStore.Max_Entries or 
       VariableStore.Has_Variable(VariableStack, var));
   
   procedure Remove(VariableStack : in out VariableStore.Database;
                    var : in VariableStore.Variable) with
     Pre =>
       VariableStore.Has_Variable(VariableStack, var);

   pragma Warnings (Off, "has no effect");
   procedure List(VariableStack : in VariableStore.Database);
   pragma Warnings (On, "has no effect");
   
end Calculator;
