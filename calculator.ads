with MyStringTokeniser;
with VariableStore;
with Ada.Containers.Vectors;


package Calculator is

   package Variable_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => VariableStore.Variable,
       "=" => VariableStore.Equal);
   subtype VariableArray is Variable_Vectors.Vector;
   use all type VariableArray;

   --  Max_Commands : constant Natural := 1024;
   --  type Index is range 1 .. Max_Commands;
   --  type LastCommandsArray is array (Index) of VariableStore.Variable;
   
   procedure Init;

   procedure Process(arg1 : in String; arg2 : in String);
   
   procedure Plus;

   procedure Minus;
   
   procedure Multiply;

   procedure Divide;

   procedure Push(value : in Integer);

   procedure Pop;

   procedure Load(var : VariableStore.Variable);
   
   procedure Store(var : in VariableStore.Variable);
   
   procedure Remove(var : in VariableStore.Variable);

   procedure List;
   
end Calculator;
