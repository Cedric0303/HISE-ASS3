with PIN;
with Ada.Text_IO; use Ada.Text_IO;

package body lock is
   Locked : Boolean := True;
   CurrentPIN : PIN.PIN;

   procedure Lock(s: in String) is
   begin
      if CheckInvalidPIN(s) then
         return;
      end if;

      CurrentPIN := PIN.From_String(s);
      Locked := True;

   end Lock;

   procedure Unlock(s: in String) is
   begin
      if CheckInvalidPIN(s) then
         return;
      end if;
      
      if PIN."="(CurrentPIN, PIN.From_String(s)) then
         Locked := False;
      end if;
   end Unlock;

   function IsLocked return Boolean is
   begin
      return Locked;
   end IsLocked;

   function CheckInvalidPIN(s: in String) return Boolean is
   begin
      if s'Length /= 4 then
         Put_Line("Invalid PIN.");
         return True;
      end if;

      for I in s'Range loop
         if s(I) < '0' or s(I) > '9' then
            Put_Line("Invalid PIN.");
            return True;
         end if;

         pragma Loop_Invariant (for all J in s'First..I => (s(J) >= '0' and s(J) <= '9'));

      end loop;

      return False;
   end CheckInvalidPIN;

end lock;
