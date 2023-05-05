with Ada.Characters.Latin_1;

package MyStringTokeniser with SPARK_Mode is

   type TokenExtent is record
      Start : Positive;
      Length : Natural;
   end record;

   type TokenArray is array(Positive range <>) of TokenExtent;

   function Is_Whitespace(Ch : Character) return Boolean is
     (Ch = ' ' or Ch = Ada.Characters.Latin_1.LF or
        Ch = Ada.Characters.Latin_1.HT);

   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) with
     Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,
     Post => Count <= Tokens'Length and
     -- Count <= Tokens'Length
     -- ensure 'Count' returned is never more than the 'Token' array
     -- length, to prevent reading elements not in the 'Token' array
     (for all Index in Tokens'First..Tokens'First+(Count-1) =>
        (Tokens(Index).Start >= S'First and
         -- Tokens(Index).Start >= S'First
         -- ensure the first letter of the each token corresponds with
         -- with the starting character of the continuously-truncated
         -- input string (with whitespace removed)
         -- to prevent creating invalid tokens
           Tokens(Index).Length > 0) and then
           -- Tokens(Index).Length > 0)
           -- ensure ...
           -- to prevent ...
        Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start);
        -- Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start
        -- ensure ...
        -- to prevent ...


end MyStringTokeniser;
