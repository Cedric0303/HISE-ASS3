with MyStringTokeniser;
with VariableStore;

package Calculator with SPARK_Mode is
   MAX_COMMANDS : constant Natural := 1024;
   type CommandArray is array (1..MAX_COMMANDS) of VariableStore.Variable;
   use type CommandArray;
     
   procedure Plus(DB : in out VariableStore.Database;
                  CA : in out CommandArray;
                  NumCommands : in out Natural;
                  Increment : in out Natural) with
     Pre =>
       ((NumCommands > CA'First + 1 and NumCommands <= CA'Last) and then
       (VariableStore.Has_Variable(DB, CA(NumCommands - 2)) and
       VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) and
       Increment < MAX_COMMANDS - 1,
       Post =>
         NumCommands in CA'Range and
         Increment < MAX_COMMANDS;
         
   procedure Minus(DB : in out VariableStore.Database;
                   CA : in out CommandArray;
                   NumCommands : in out Natural;
                   Increment : in out Natural) with
     Pre =>
       ((NumCommands > CA'First + 1 and NumCommands <= CA'Last) and then
       (VariableStore.Has_Variable(DB, CA(NumCommands - 2)) and
       VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) and
       Increment < MAX_COMMANDS - 1,
       Post =>
         NumCommands in CA'Range and
         Increment < MAX_COMMANDS;
   
   procedure Multiply(DB : in out VariableStore.Database;
                      CA : in out CommandArray;
                      NumCommands : in out Natural;
                      Increment : in out Natural) with
     Pre =>
       ((NumCommands > CA'First + 1 and NumCommands <= CA'Last) and then
       (VariableStore.Has_Variable(DB, CA(NumCommands - 2)) and
       VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) and
       Increment < MAX_COMMANDS - 1,
       Post =>
         NumCommands in CA'Range and
         Increment < MAX_COMMANDS;
   
   procedure Divide(DB : in out VariableStore.Database;
                    CA : in out CommandArray;
                    NumCommands : in out Natural;
                    Increment : in out Natural) with
     Pre =>
       ((NumCommands > CA'First + 1 and NumCommands <= CA'Last) and then
       (VariableStore.Has_Variable(DB, CA(NumCommands - 2)) and
       VariableStore.Has_Variable(DB, CA(NumCommands - 1)))) and
       Increment < MAX_COMMANDS - 1,
       Post =>
         NumCommands in CA'Range and
         Increment < MAX_COMMANDS;
   
   procedure Push(DB : in out VariableStore.Database;
                  CA : in out CommandArray;
                  value : in Integer;
                  NumCommands : in out Natural;
                  Increment : in out Natural) with
     Pre => 
       NumCommands in CA'Range and
       NumCommands < CA'Last - 1 and
       Increment < MAX_COMMANDS - 1,
       Post =>
         NumCommands in CA'Range and
         Increment < MAX_COMMANDS;
   
   procedure Pop(DB : in out VariableStore.Database;
                 CA : in CommandArray;
                 NumCommands : in out Natural) with
     Pre => 
       (NumCommands > CA'First and NumCommands < CA'Last - 1) and then
       (VariableStore.Has_Variable(DB, CA(NumCommands - 1))),
       Post =>
         NumCommands in CA'Range;
   
   procedure Load(DB : in out VariableStore.Database;
                  CA : in out CommandArray;
                  old_var : VariableStore.Variable;
                  NumCommands : in out Natural;
                  Increment : in out Natural) with
     Pre => 
       NumCommands in CA'Range and
       VariableStore.Has_Variable(DB, old_var) and
       NumCommands < CA'Last - 1 and
       Increment < MAX_COMMANDS - 1,
       Post =>
         NumCommands in CA'Range and
         Increment < MAX_COMMANDS;
   
   procedure Store(DB : in out VariableStore.Database;
                   CA : in out CommandArray;
                   var : in VariableStore.Variable;
                   NumCommands : in out Natural) with
     Pre => 
       (NumCommands > CA'First + 1 and NumCommands - 1 < CA'Last) and then
       VariableStore.Has_Variable(DB, CA(NumCommands - 1)),
       Post =>
         NumCommands in CA'Range;
   
   procedure Remove(DB : in out VariableStore.Database;
                    CA : in out CommandArray;
                    var : in VariableStore.Variable;
                    NumCommands : in out Natural) with
     Pre => 
       NumCommands in CA'Range and
       VariableStore.Has_Variable(DB, var) and
       NumCommands > CA'First,
       Post =>
         NumCommands in CA'Range;
   
   pragma Warnings (Off, "has no effect");
   procedure List(DB : in VariableStore.Database);
   pragma Warnings (On, "has no effect");
   
end Calculator;
