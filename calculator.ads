with MyStringTokeniser;
with VariableStore;

package Calculator is

   procedure Process(db : in out VariableStore.Database;
                     lastcommand : in out VariableStore.Variable;
                     arg1 : in String;
                     arg2 : in String;
                     done : in out Boolean);

   procedure Push(db : in out VariableStore.Database;
                  var : in VariableStore.Variable;
                  value : in Integer);
   
   procedure Pop(db : in out VariableStore.Database;
                 lastcommand : VariableStore.Variable);

   procedure List(db : in VariableStore.Database);
end Calculator;
