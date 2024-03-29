
package body MyStringTokeniser with SPARK_Mode is



   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) is
      Index : Positive;
      Extent : TokenExtent;
      OutIndex : Integer := Tokens'First;
   begin
      Count := 0;
      if (S'First > S'Last) then
         return;
      end if;
      Index := S'First;
      while OutIndex <= Tokens'Last and Index <= S'Last and Count < Tokens'Length loop
         pragma Loop_Invariant
           (for all J in Tokens'First..OutIndex-1 =>
              (Tokens(J).Start >= S'First and
              -- ensure the first letter of the each token corresponds with
              -- with the starting character of the continuously-truncated
              -- input string (with whitespace removed)
                   Tokens(J).Length > 0) and then
                   -- 
            Tokens(J).Length-1 <= S'Last - Tokens(J).Start);
         -- before the loop, at the beginning, middle and end of the loop,
         -- for all tokens in the 'Tokens' array:
         -- the first letter of each token corresponds with the starting
         -- character of the continuously-truncated input string 
         -- (with whitespace removed), and each token's length is always 
         -- valid (non-empty)

         pragma Loop_Invariant (OutIndex = Tokens'First + Count);
         -- before the loop, at the beginning, middle and end of the loop,
         -- the returned output index is always the first element of the 
         -- 'Token' array plus the number of tokens found from inside the loop,
         -- ensures the output index corresponds with the number of elements
         -- in the 'Token' array

         -- look for start of next token
         while (Index >= S'First and Index < S'Last) and then Is_Whitespace(S(Index)) loop
            Index := Index + 1;
         end loop;
         if (Index >= S'First and Index <= S'Last) and then not Is_Whitespace(S(Index)) then
            -- found a token
            Extent.Start := Index;
            Extent.Length := 0;

            -- look for end of this token
            while Positive'Last - Extent.Length >= Index and then (Index+Extent.Length >= S'First and Index+Extent.Length <= S'Last) and then not Is_Whitespace(S(Index+Extent.Length)) loop
               Extent.Length := Extent.Length + 1;
            end loop;

            Tokens(OutIndex) := Extent;
            Count := Count + 1;

            -- check for last possible token, avoids overflow when incrementing OutIndex
            if (OutIndex = Tokens'Last) then
               return;
            else
               OutIndex := OutIndex + 1;
            end if;

            -- check for end of string, avoids overflow when incrementing Index
            if S'Last - Extent.Length < Index then
               return;
            else
               Index := Index + Extent.Length;
            end if;
         end if;
      end loop;
   end Tokenise;

end MyStringTokeniser;
