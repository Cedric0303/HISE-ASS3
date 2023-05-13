with MyStringTokeniser;
with VariableStore;

package Calculator with SPARK_Mode is
   MAX_COMMANDS : constant Natural := 1024;
   type CommandArray is array (1..MAX_COMMANDS) of VariableStore.Variable;
   use type CommandArray;
   
   NumCommands : Natural := 1;
   Increment : Natural := 0;

   procedure Process(DB : in out VariableStore.Database;
                     CA : in out CommandArray;
                     arg1 : in String; 
                     arg2 : in String);
   
   procedure Plus(DB : in out VariableStore.Database;
                  CA : in out CommandArray);

   procedure Minus(DB : in out VariableStore.Database;
                   CA : in out CommandArray);
   
   procedure Multiply(DB : in out VariableStore.Database;
                      CA : in out CommandArray);

   procedure Divide(DB : in out VariableStore.Database;
                    CA : in out CommandArray);

   procedure Push(DB : in out VariableStore.Database;
                  CA : in out CommandArray;
                  value : in Integer);

   procedure Pop(DB : in out VariableStore.Database;
                 CA : in out CommandArray);

   procedure Load(DB : in out VariableStore.Database;
                  CA : in out CommandArray;
                  var : VariableStore.Variable);
   
   procedure Store(DB : in out VariableStore.Database;
                   CA : in out CommandArray;
                   var : in VariableStore.Variable);
   
   procedure Remove(DB : in out VariableStore.Database;
                    CA : in out CommandArray;
                    var : in VariableStore.Variable);

   procedure List(DB : in out VariableStore.Database);
end Calculator;
