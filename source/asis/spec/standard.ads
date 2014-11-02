------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

package Standard is

   pragma Pure (Standard);

   type Boolean is (False, True);

   -- The predefined relational operators for this type are as follows:

   -- function "="   (Left, Right : Boolean'Base) return Boolean;
   -- function "/="  (Left, Right : Boolean'Base) return Boolean;
   -- function "<"   (Left, Right : Boolean'Base) return Boolean;
   -- function "<="  (Left, Right : Boolean'Base) return Boolean;
   -- function ">"   (Left, Right : Boolean'Base) return Boolean;
   -- function ">="  (Left, Right : Boolean'Base) return Boolean;

   -- The predefined logical operators and the predefined logical
   -- negation operator are as follows:

   -- function "and" (Left, Right : Boolean'Base) return Boolean;
   -- function "or"  (Left, Right : Boolean'Base) return Boolean;
   -- function "xor" (Left, Right : Boolean'Base) return Boolean;

   -- function "not" (Right : Boolean'Base) return Boolean;

   -- The integer type root_integer and the
   -- corresponding universal type universal_integer are predefined.

   type Integer is range implementation-defined .. implementation-defined;

   subtype Natural  is Integer range 0 .. Integer'Last;
   subtype Positive is Integer range 1 .. Integer'Last;

   -- The predefined operators for type Integer are as follows:

   -- function "="  (Left, Right : Integer'Base) return Boolean;
   -- function "/=" (Left, Right : Integer'Base) return Boolean;
   -- function "<"  (Left, Right : Integer'Base) return Boolean;
   -- function "<=" (Left, Right : Integer'Base) return Boolean;
   -- function ">"  (Left, Right : Integer'Base) return Boolean;
   -- function ">=" (Left, Right : Integer'Base) return Boolean;

   -- function "+"   (Right : Integer'Base) return Integer'Base;
   -- function "-"   (Right : Integer'Base) return Integer'Base;
   -- function "abs" (Right : Integer'Base) return Integer'Base;

   -- function "+"   (Left, Right : Integer'Base) return Integer'Base;
   -- function "-"   (Left, Right : Integer'Base) return Integer'Base;
   -- function "*"   (Left, Right : Integer'Base) return Integer'Base;
   -- function "/"   (Left, Right : Integer'Base) return Integer'Base;
   -- function "rem" (Left, Right : Integer'Base) return Integer'Base;
   -- function "mod" (Left, Right : Integer'Base) return Integer'Base;

   -- function "**"  (Left : Integer'Base; Right : Natural)
   --                  return Integer'Base;

   -- The specification of each operator for the type
   -- root_integer, or for any additional predefined integer
   -- type, is obtained by replacing Integer by the name of the type
   -- in the specification of the corresponding operator of the type
   -- Integer. The right operand of the exponentiation operator
   -- remains as subtype Natural.

   -- The floating point type root_real and the
   -- corresponding universal type universal_real are predefined.

   type Float is digits implementation-defined;

   -- The predefined operators for this type are as follows:

   -- function "="   (Left, Right : Float) return Boolean;
   -- function "/="  (Left, Right : Float) return Boolean;
   -- function "<"   (Left, Right : Float) return Boolean;
   -- function "<="  (Left, Right : Float) return Boolean;
   -- function ">"   (Left, Right : Float) return Boolean;
   -- function ">="  (Left, Right : Float) return Boolean;

   -- function "+"   (Right : Float) return Float;
   -- function "-"   (Right : Float) return Float;
   -- function "abs" (Right : Float) return Float;

   -- function "+"   (Left, Right : Float) return Float;
   -- function "-"   (Left, Right : Float) return Float;
   -- function "*"   (Left, Right : Float) return Float;
   -- function "/"   (Left, Right : Float) return Float;

   -- function "**"  (Left : Float; Right : Integer'Base) return Float;

   -- The specification of each operator for the type root_real, or for
   -- any additional predefined floating point type, is obtained by
   -- replacing Float by the name of the type in the specification of the
   -- corresponding operator of the type Float.

   -- In addition, the following operators are predefined for the root
   -- numeric types:

   -- function "*" (Left : root_integer; Right : root_real)
   --   return root_real;

   -- function "*" (Left : root_real;    Right : root_integer)
   --   return root_real;

   -- function "/" (Left : root_real;    Right : root_integer)
   --   return root_real;

   -- The type universal_fixed is predefined.
   -- The only multiplying operators defined between
   -- fixed point types are

   -- function "*" (Left : universal_fixed; Right : universal_fixed)
   --   return universal_fixed;

   -- function "/" (Left : universal_fixed; Right : universal_fixed)
   --   return universal_fixed;

   -- The type universal_access is predefined.
   -- The following equality operators are predefined:

   --  function "="  (Left, Right: universal_access) return Boolean;
   --  function "/=" (Left, Right: universal_access) return Boolean;

   -- The declaration of type Character is based on the standard ISO 8859-1
   -- character set.

   -- There are no character literals corresponding to the positions for
   -- control characters.
   -- They are indicated in italics in this definition. See 3.5.2.

   type Character is ('x');

   -- The predefined operators for the type Character are the same as for
   -- any enumeration type.

   -- The declaration of type Wide_Character is based on the standard
   -- ISO/IEC 10646:2003 BMP character set.
   -- The first 256 positions have the same contents as type Character.
   -- See 3.5.2.

   type Wide_Character is ('x');

   -- The declaration of type Wide_Wide_Character is based on the full
   -- ISO/IEC 10646:2003 character set. The first 65536 positions have the
   -- same contents as type Wide_Character. See 3.5.2.

   type Wide_Wide_Character is ('x');
   for Wide_Wide_Character'Size use 32;

   package ASCII is  --  Obsolescent; see J.5

      --  Control characters:

      NUL   : constant Character := Character'Val(0);
      SOH   : constant Character := Character'Val(1);
      STX   : constant Character := Character'Val(2);
      ETX   : constant Character := Character'Val(3);
      EOT   : constant Character := Character'Val(4);
      ENQ   : constant Character := Character'Val(5);
      ACK   : constant Character := Character'Val(6);
      BEL   : constant Character := Character'Val(7);
      BS    : constant Character := Character'Val(8);
      HT    : constant Character := Character'Val(9);
      LF    : constant Character := Character'Val(10);
      VT    : constant Character := Character'Val(11);
      FF    : constant Character := Character'Val(12);
      CR    : constant Character := Character'Val(13);
      SO    : constant Character := Character'Val(14);
      SI    : constant Character := Character'Val(15);

      DLE   : constant Character := Character'Val(16);
      DC1   : constant Character := Character'Val(17);
      DC2   : constant Character := Character'Val(18);
      DC3   : constant Character := Character'Val(19);
      DC4   : constant Character := Character'Val(20);
      NAK   : constant Character := Character'Val(21);
      SYN   : constant Character := Character'Val(22);
      ETB   : constant Character := Character'Val(23);
      CAN   : constant Character := Character'Val(24);
      EM    : constant Character := Character'Val(25);
      SUB   : constant Character := Character'Val(26);
      ESC   : constant Character := Character'Val(27);
      FS    : constant Character := Character'Val(28);
      GS    : constant Character := Character'Val(29);
      RS    : constant Character := Character'Val(30);
      US    : constant Character := Character'Val(31);

      DEL   : constant Character := Character'Val(127);

      -- Other characters:

      Exclam     : constant Character:= '!';
      Quotation  : constant Character:= '"';
      Sharp      : constant Character:= '#';
      Dollar     : constant Character:= '$';
      Percent    : constant Character:= '%';
      Ampersand  : constant Character:= '&';
      Colon      : constant Character:= ':';
      Semicolon  : constant Character:= ';';
      Query      : constant Character:= '?';
      At_Sign    : constant Character:= '@';
      L_Bracket  : constant Character:= '[';
      Back_Slash : constant Character:= '\';
      R_Bracket  : constant Character:= ']';
      Circumflex : constant Character:= '^';
      Underline  : constant Character:= '_';
      Grave      : constant Character:= '`';
      L_Brace    : constant Character:= '{';
      Bar        : constant Character:= '|';
      R_Brace    : constant Character:= '}';
      Tilde      : constant Character:= '~';

      -- Lower case letters:

      LC_A : constant Character:= 'a';
      LC_B : constant Character:= 'b';
      LC_C : constant Character:= 'c';
      LC_D : constant Character:= 'd';
      LC_E : constant Character:= 'e';
      LC_F : constant Character:= 'f';
      LC_G : constant Character:= 'g';
      LC_H : constant Character:= 'h';
      LC_I : constant Character:= 'i';
      LC_J : constant Character:= 'j';
      LC_K : constant Character:= 'k';
      LC_L : constant Character:= 'l';
      LC_M : constant Character:= 'm';
      LC_N : constant Character:= 'n';
      LC_O : constant Character:= 'o';
      LC_P : constant Character:= 'p';
      LC_Q : constant Character:= 'q';
      LC_R : constant Character:= 'r';
      LC_S : constant Character:= 's';
      LC_T : constant Character:= 't';
      LC_U : constant Character:= 'u';
      LC_V : constant Character:= 'v';
      LC_W : constant Character:= 'w';
      LC_X : constant Character:= 'x';
      LC_Y : constant Character:= 'y';
      LC_Z : constant Character:= 'z';

   end ASCII;

   -- Predefined string types:

   type String is array (Positive range <>) of Character;
   pragma Pack (String);

   -- The predefined operators for this type are as follows:

   --     function "="  (Left, Right: String) return Boolean;
   --     function "/=" (Left, Right: String) return Boolean;
   --     function "<"  (Left, Right: String) return Boolean;
   --     function "<=" (Left, Right: String) return Boolean;
   --     function ">"  (Left, Right: String) return Boolean;
   --     function ">=" (Left, Right: String) return Boolean;

   --     function "&" (Left: String;    Right: String)    return String;
   --     function "&" (Left: Character; Right: String)    return String;
   --     function "&" (Left: String;    Right: Character) return String;
   --     function "&" (Left: Character; Right: Character) return String;

   type Wide_String is array (Positive range <>) of Wide_Character;
   pragma Pack (Wide_String);

   -- The predefined operators for this type correspond to those for String

   type Wide_Wide_String is array (Positive range <>) of Wide_Wide_Character;
   pragma Pack (Wide_Wide_String);

   -- The predefined operators for this type correspond to those for String.

   type Duration is delta implementation-defined
     range implementation-defined .. implementation-defined;

      -- The predefined operators for the type Duration are the same as for
      -- any fixed point type.

   -- The predefined exceptions:

   Constraint_Error : exception;
   Program_Error    : exception;
   Storage_Error    : exception;
   Tasking_Error    : exception;

   --  Obsolescent; see J.6 Numeric_Error:
   Numeric_Error : exception renames Constraint_Error;
end Standard;




