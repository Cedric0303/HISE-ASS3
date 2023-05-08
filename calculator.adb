with Ada.Text_IO; use Ada.Text_IO;

--  pragma Ada_2012;
package body Calculator is
   UNLOCKED : Boolean := false;
   DONE : Boolean := false;
   -------------
   -- Process --
   -------------

   procedure Process (arg1: in String;
                      arg2: in String;
                      done : in out Boolean) is
   begin
      Put("1: "); Put_Line(arg1);
      Put("2: "); Put_Line(arg2);
      done := True;
      return;
   end Process;

end Calculator;
