with MyStringTokeniser;
with VariableStore;
with Ada.Containers; use Ada.Containers;

package Calculator with
 SPARK_Mode
is

  procedure Plus (ValueStack : in out VariableStore.Database; IsLocked : in Boolean) with
     Pre =>
       not IsLocked and
    Integer (VariableStore.Length (ValueStack)) >= 2 and
    VariableStore.Has_Variable
     (ValueStack,
      VariableStore.From_String
       (Integer (Integer (VariableStore.Length (ValueStack)) - 2)'Image)) and
    VariableStore.Has_Variable
     (ValueStack,
      VariableStore.From_String
        (Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image));

  procedure Minus (ValueStack : in out VariableStore.Database; IsLocked : in Boolean) with
   Pre =>
       not IsLocked and
    Integer (VariableStore.Length (ValueStack)) >= 2 and
    VariableStore.Has_Variable
     (ValueStack,
      VariableStore.From_String
       (Integer (Integer (VariableStore.Length (ValueStack)) - 2)'Image)) and
    VariableStore.Has_Variable
     (ValueStack,
      VariableStore.From_String
       (Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image));

  procedure Multiply (ValueStack : in out VariableStore.Database; IsLocked : in Boolean) with
   Pre =>
       not IsLocked and
    Integer (VariableStore.Length (ValueStack)) >= 2 and
    VariableStore.Has_Variable
     (ValueStack,
      VariableStore.From_String
       (Integer (Integer (VariableStore.Length (ValueStack)) - 2)'Image)) and
    VariableStore.Has_Variable
     (ValueStack,
      VariableStore.From_String
       (Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image));

  procedure Divide (ValueStack : in out VariableStore.Database; IsLocked : in Boolean) with
   Pre =>
       not IsLocked and
    Integer (VariableStore.Length (ValueStack)) >= 2 and
    VariableStore.Has_Variable
     (ValueStack,
      VariableStore.From_String
       (Integer (Integer (VariableStore.Length (ValueStack)) - 2)'Image)) and
    VariableStore.Has_Variable
     (ValueStack,
      VariableStore.From_String
       (Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image));

  procedure Push
   (ValueStack : in out VariableStore.Database; value : in Integer; IsLocked : in Boolean) with
   Pre =>
       not IsLocked and
    (VariableStore.Length (ValueStack) < 512 or
    VariableStore.Has_Variable
     (ValueStack,
      VariableStore.From_String
       (Integer (Integer (VariableStore.Length (ValueStack)))'Image)));

  procedure Pop (ValueStack : in out VariableStore.Database; IsLocked : in Boolean) with
   Pre =>
       not IsLocked and
    VariableStore.Has_Variable
     (ValueStack,
      VariableStore.From_String
       (Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image));

  procedure Load
   (ValueStack    : in out VariableStore.Database;
    VariableStack : in     VariableStore.Database;
    OldVar        : in     VariableStore.Variable;
    IsLocked : in Boolean) with
   Pre =>
       not IsLocked and
    VariableStore.Has_Variable (VariableStack, OldVar) and
    (VariableStore.Length (ValueStack) < VariableStore.Max_Entries or
     VariableStore.Has_Variable
      (ValueStack,
       VariableStore.From_String
        (Integer (Integer (VariableStore.Length (ValueStack)))'Image)));

  procedure Store
   (ValueStack    : in out VariableStore.Database;
    VariableStack : in out VariableStore.Database;
    var           : in     VariableStore.Variable;
    IsLocked : in Boolean) with
   Pre =>
       not IsLocked and
    VariableStore.Has_Variable
     (ValueStack,
      VariableStore.From_String
       (Integer (Integer (VariableStore.Length (ValueStack)) - 1)'Image)) and
    (VariableStore.Length (VariableStack) < VariableStore.Max_Entries or
     VariableStore.Has_Variable (VariableStack, var));

  procedure Remove
   (VariableStack : in out VariableStore.Database;
    var           : in     VariableStore.Variable;
    IsLocked : in Boolean) with
   Pre => not IsLocked and VariableStore.Has_Variable (VariableStack, var);

end Calculator;
