with MyStringTokeniser;
with VariableStore;
with IntegerToString;

package Calculator with SPARK_Mode is
   MAX_COMMANDS : constant Natural := 1024;
   type CommandArray is array (1..MAX_COMMANDS) of VariableStore.Variable;
   use type CommandArray;
   
   NumCommands : Natural := 1;
   Increment : Natural := 0;
   
   procedure Plus(DB : in out VariableStore.Database;
                  CA : in out CommandArray) with
     Pre =>
       NumCommands > 2 and 
       NumCommands < MAX_COMMANDS and
       ((NumCommands - 2 > 0 and NumCommands - 2 < MAX_COMMANDS) and then
       (VariableStore.Has_Variable(DB, CA(NumCommands - 2)) and
       VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) and
       IntegerToString.To_String(Increment)'Length <= VariableStore.Max_Variable_Length and
       ((Increment <= Integer'Last - 1) and then (Increment + 1) in Integer);
         
   procedure Minus(DB : in out VariableStore.Database;
                   CA : in out CommandArray) with
     Pre =>
       NumCommands > 2 and 
       NumCommands < MAX_COMMANDS and
       ((NumCommands - 2 > 0 and NumCommands - 2 < MAX_COMMANDS) and then
       (VariableStore.Has_Variable(DB, CA(NumCommands - 2)) and
       VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) and
       IntegerToString.To_String(Increment)'Length <= VariableStore.Max_Variable_Length and
       ((Increment <= Integer'Last - 1) and then (Increment + 1) in Integer);
   
   procedure Multiply(DB : in out VariableStore.Database;
                      CA : in out CommandArray) with
     Pre =>
       NumCommands > 2 and 
       NumCommands < MAX_COMMANDS and
       ((NumCommands - 2 > 0 and NumCommands - 2 < MAX_COMMANDS) and then
       (VariableStore.Has_Variable(DB, CA(NumCommands - 2)) and
       VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) and
       IntegerToString.To_String(Increment)'Length <= VariableStore.Max_Variable_Length and
       ((Increment <= Integer'Last - 1) and then (Increment + 1) in Integer);
   
   procedure Divide(DB : in out VariableStore.Database;
                    CA : in out CommandArray) with
     Pre =>
       NumCommands > 2 and 
       NumCommands < MAX_COMMANDS and
       ((NumCommands - 2 > 0 and NumCommands - 2 < MAX_COMMANDS) and then
       (VariableStore.Has_Variable(DB, CA(NumCommands - 2)) and
       VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) and
       IntegerToString.To_String(Increment)'Length <= VariableStore.Max_Variable_Length and
       ((Increment <= Integer'Last - 1) and then (Increment + 1) in Integer);
   
   procedure Push(DB : in out VariableStore.Database;
                  CA : in out CommandArray;
                  value : in Integer) with
     Pre => 
       NumCommands in CA'Range and
       IntegerToString.To_String(Increment)'Length <= VariableStore.Max_Variable_Length and
       ((Increment <= Integer'Last - 1) and then (Increment + 1) in Integer);
   
   procedure Pop(DB : in out VariableStore.Database;
                 CA : in CommandArray) with
     Pre => 
       NumCommands in CA'Range and
       ((NumCommands - 1 > 0 and NumCommands - 1 < MAX_COMMANDS) and then
       (VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) and 
       IntegerToString.To_String(Increment)'Length <= VariableStore.Max_Variable_Length and
       ((Increment <= Integer'Last - 1) and then (Increment + 1) in Integer);
   
   procedure Load(DB : in out VariableStore.Database;
                  CA : in out CommandArray;
                  old_var : VariableStore.Variable) with
     Pre => 
       NumCommands in CA'Range and
       VariableStore.Has_Variable(DB, old_var) and
       IntegerToString.To_String(Increment)'Length <= VariableStore.Max_Variable_Length and
       ((Increment <= Integer'Last - 1) and then (Increment + 1) in Integer);
   
   procedure Store(DB : in out VariableStore.Database;
                   CA : in out CommandArray;
                   var : in VariableStore.Variable) with
     Pre => 
       NumCommands > 1 and 
       NumCommands < MAX_COMMANDS and
       ((NumCommands - 1 > 0 and NumCommands - 1 < MAX_COMMANDS) and then
       (VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) and
       IntegerToString.To_String(Increment)'Length <= VariableStore.Max_Variable_Length and
       ((Increment <= Integer'Last - 1) and then (Increment + 1) in Integer);
   
   procedure Remove(DB : in out VariableStore.Database;
                    CA : in out CommandArray;
                    var : in VariableStore.Variable) with
     Pre => 
       NumCommands in CA'Range and
       VariableStore.Has_Variable(DB, var);
   
   pragma Warnings (Off, "has no effect");
   procedure List(DB : in VariableStore.Database);
   pragma Warnings (On, "has no effect");
   
end Calculator;
