with MyStringTokeniser;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package CommandLineActions with SPARK_Mode is

   procedure ProcessLine(S : in String; command : out Unbounded_String; arg : out Unbounded_String) with
     Pre => S'Length <= 2048;

end CommandLineActions;
