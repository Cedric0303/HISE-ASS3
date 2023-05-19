with PIN;

package body Lock with SPARK_Mode is
   Locked : Boolean := True;

   procedure Lock(CurrentPIN: out PIN.PIN; s: in String) is
   begin
      CurrentPIN := PIN.From_String("0000");

      if s'Length /= 4 then
         return;
      end if;

      for I in s'Range loop
         if s(I) < '0' or s(I) > '9' then
            return;
         end if;

         pragma Loop_Invariant (for all J in s'First..I => (s(J) >= '0' and s(J) <= '9'));

      end loop;

      CurrentPIN := PIN.From_String(s);
      Locked := True;
   end Lock;

   procedure Unlock(CurrentPIN: in PIN.PIN;
                    s: in String) is
   begin
      if s'Length /= 4 then
         return;
      end if;

      for I in s'Range loop
         if s(I) < '0' or s(I) > '9' then
            return;
         end if;

         pragma Loop_Invariant (for all J in s'First..I => (s(J) >= '0' and s(J) <= '9'));

      end loop;
      
      if PIN."="(CurrentPIN, PIN.From_String(s)) then
         Locked := False;
      end if;
   end Unlock;

   function IsLocked return Boolean is
   begin
      return Locked;
   end IsLocked;

   function IsInvalidPIN(s: in String) return Boolean is
   begin
      if s'Length /= 4 then
         return True;
      end if;

      for I in s'Range loop
         if s(I) < '0' or s(I) > '9' then
            return True;
         end if;

         pragma Loop_Invariant (for all J in s'First..I => (s(J) >= '0' and s(J) <= '9'));

      end loop;

      return False;
   end IsInvalidPIN;

end Lock;
