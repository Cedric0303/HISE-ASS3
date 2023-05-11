package lock is

   procedure Lock(s: in String);

   procedure Unlock(s: in String);

   function IsLocked return Boolean;

   private function CheckInvalidPIN(s : in String) return Boolean;

end lock;
