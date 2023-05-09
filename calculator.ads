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

   procedure Process(db : in out VariableStore.Database;
                     commandarray : in out VariableArray;
                     arg1 : in String;
                     arg2 : in String;
                     done : in out Boolean);
   
   procedure Plus(db : in out VariableStore.Database;
                  commandarray : in out VariableArray);

   procedure Minus(db : in out VariableStore.Database;
                   commandarray : in out VariableArray);
   procedure Multiply(db : in out VariableStore.Database;
                      commandarray : in out VariableArray);

   procedure Divide(db : in out VariableStore.Database;
                    commandarray : in out VariableArray);

   procedure Push(db : in out VariableStore.Database;
                  commandarray : in out VariableArray;
                  value : in Integer);

   procedure Pop(db : in out VariableStore.Database;
                 commandarray : in out VariableArray);

      procedure Load(db : in out VariableStore.Database;
                     commandarray : in out VariableArray;
                     var : VariableStore.Variable);
   
   procedure Store(db : in out VariableStore.Database;
                   commandarray : in out VariableArray;
                   var : in VariableStore.Variable);
   
   procedure Remove(db : in out VariableStore.Database;
                    commandarray : in out VariableArray;
                    var : in VariableStore.Variable);

   procedure List(db : in VariableStore.Database);
end Calculator;
