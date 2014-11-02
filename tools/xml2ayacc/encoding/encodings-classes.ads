--------------------------------------------------------
--                E n c o d i n g s                   --
--                                                    --
-- Tools for convertion strings between Unicode and   --
-- national/vendor character sets.                    --
--                - - - - - - - - -                   --
-- Read copyright and license at the end of this file --
--------------------------------------------------------

--  This package is intended for fast calculation equivalence classes
--  for decoded characters.
--  It use map table for 8-bit encodings and cache for UTF-8.
--  User provides Get_Class function to determine
--  equivalence classes of a wide character.
--  If supplied character is begin of surrogate character
--  function should return Unknown.
--  Next time pair of characters would be supplied to
--  determine equivalence class and so on.

generic
   type Character_Class is mod <>;
   with function Get_Class (C : Wide_String) return Character_Class;
package Encodings.Classes is

   Unknown : constant Character_Class := Character_Class'Last;

   type Character_Classes is array (Positive range <>) of Character_Class;
   pragma Pack (Character_Classes);

   type Coder (Map : Encoding := Encodings.UTF_8) is private;

   procedure Decode
     (Text        : in     Raw_String;
      Text_Last   :    out Natural;
      Result      :    out Wide_String;
      Result_Last :    out Natural;
      Classes     :    out Character_Classes;
      Object      : in out Coder);

private

   Cache_Size  : constant := 257;

   subtype Cache_Index is Positive range 1 .. Cache_Size;
   subtype Class_Cache is Character_Classes (Cache_Index);
   type Class_Map      is array (Character) of Character_Class;

   type Coder (Map : Encoding := Encodings.UTF_8) is record
      Prefix        : Wide_Character := ' ';
      case Map is
         when Encodings.Unknown =>
            null;
         when UTF_8 =>
            Cache   : Class_Cache := (others => Unknown);
            Wide    : Wide_String (Cache_Index);
         when others =>  --  8 bit encoding
            Classes : Class_Map   := (others => Unknown);
      end case;
   end record;

end Encodings.Classes;


------------------------------------------------------------------------------
--  Copyright (c) 2006-2013, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the Maxim Reznik, IE nor the names of its
--       contributors may be used to endorse or promote products derived from
--       this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------
