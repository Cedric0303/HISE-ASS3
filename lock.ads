with PIN;

package Lock with SPARK_Mode is

   procedure Lock(CurrentPIN: out PIN.PIN; s: in String);

   procedure Unlock(CurrentPIN: in PIN.PIN; s: in String);

   function IsLocked return Boolean;

   private function CheckInvalidPIN(s : in String) return Boolean;

end Lock;
