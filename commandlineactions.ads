with MyStringTokeniser;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package CommandLineActions is

   procedure PutState(locked : in Boolean);

   procedure ProcessLine(T : in out MyStringTokeniser.TokenArray; command : out Unbounded_String; arg : out Unbounded_String);

end CommandLineActions;