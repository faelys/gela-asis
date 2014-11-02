with Gela.Classificators;
with Gela.Character_Class_Buffers; use Gela;
with Asis.Gela.Parser.Tokens;

package Asis.Gela.Scanner_Tables is
   subtype Token is Asis.Gela.Parser.Tokens.Token;
   Error : constant Token := Asis.Gela.Parser.Tokens.Error;

   subtype Character_Class is Character_Class_Buffers.Character_Class
     range 0 .. 57;

   type State is mod 359;

   Default              : constant State := 0;
   Allow_Char           : constant State := 334;
   Allow_Keyword        : constant State := 357;

   function Switch (S : State; C : Character_Class) return State;
   pragma Inline (Switch);

   function Accepted (S : State) return Token;
   pragma Inline (Accepted);


   subtype Code_Point is Classificators.Code_Point;

   function Get_Class (Pos : Code_Point) return Character_Class;

end Asis.Gela.Scanner_Tables;
