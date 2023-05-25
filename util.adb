package body util with SPARK_Mode is

   function checkOverflow(val1: Integer; val2: Integer) return Boolean is
   begin
      if ((val1 >= 0 and then val2 >= Integer'Last - val1) or else
            (val1 < 0 and then val2 <= Integer'First - val1))  then
         return true;
      end if;
      return false;
   end;

end util;
