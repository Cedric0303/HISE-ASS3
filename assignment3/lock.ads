with PIN;

package Lock with
 SPARK_Mode
is

  procedure Lock(CurrentPIN : out PIN.PIN; s : in String; IsLocked : in out Boolean) with
     Pre => (not IsLocked and s' Length = 4 and (for all I in s'Range => s(I) >= '0' and s(I) <= '9')),
     Post => (IsLocked and PIN."="(CurrentPIN, PIN.From_String(s)));

  procedure Unlock(CurrentPIN : in PIN.PIN; s : in String; IsLocked : in out Boolean) with
     Pre => (IsLocked and s' Length = 4 and (for all I in s'Range => s(I) >= '0' and s(I) <= '9')),
     Post => (not IsLocked and PIN."="(CurrentPIN, PIN.From_String(s))) or (IsLocked and not PIN."="(CurrentPIN, PIN.From_String(s)));

  function IsInvalidPIN(s : in String) return Boolean with
     Post =>
      (not IsInvalidPIN'Result and s'Length = 4 and (for all I in s'Range => s(I) >= '0' and s(I) <= '9')) or
      (IsInvalidPIN'Result and (s'Length /= 4 or (for some I in s'Range => s(I) < '0' or s(I) > '9')));

end Lock;
