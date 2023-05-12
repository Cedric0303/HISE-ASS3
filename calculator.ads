with MyStringTokeniser;
with VariableStore;
with Ada.Containers.Vectors;
with Ada.Strings.Bounded;    use Ada.Strings.Bounded;



package Calculator with SPARK_Mode is
   MAX_COMMANDS : constant Natural := 1024;

   type command_record_array is array (1..MAX_COMMANDS) of VariableStore.Variable;
   
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
