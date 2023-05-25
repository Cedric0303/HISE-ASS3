with PIN;

package Lock with
 SPARK_Mode
is

  procedure Lock
   (CurrentPIN : out PIN.PIN; s : in String; IsLocked : out Boolean) with
   Post => (IsLocked and PIN."=" (CurrentPIN, PIN.From_String (s)));

  procedure Unlock
   (CurrentPIN : in PIN.PIN; s : in String; IsLocked : out Boolean) with
   Post => (not IsLocked and PIN."=" (CurrentPIN, PIN.From_String (s)));

  function IsInvalidPIN (s : in String) return Boolean;

end Lock;
