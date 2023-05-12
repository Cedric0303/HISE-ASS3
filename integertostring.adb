with Ada.Strings.Fixed;

package body IntegerToString with SPARK_Mode Is
   function To_String(I : Integer) return String is
   begin
      return Ada.Strings.Fixed.Trim(Integer'Image(I), Ada.Strings.Left);
   end To_String;
   
end IntegerToString;
