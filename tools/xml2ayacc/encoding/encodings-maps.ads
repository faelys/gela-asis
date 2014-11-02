--------------------------------------------------------
--                E n c o d i n g s                   --
--                                                    --
-- Tools for convertion strings between Unicode and   --
-- national/vendor character sets.                    --
--                - - - - - - - - -                   --
-- Read copyright and license at the end of this file --
--------------------------------------------------------

package Encodings.Maps is

   type Range_Count is range 0 .. 1024;
   subtype Range_Index is Range_Count range 1 .. Range_Count'Last;

   type Backward_Index is range 1 .. 1024;

   type Forward_Map  is array (Character range <>) of Wide_Character;
   pragma Pack (Forward_Map);

   type Backward_Map is array (Backward_Index range <>) of Character;
   pragma Pack (Backward_Map);

   type Wide_Range is record
      Lower : Wide_Character;
      Upper : Wide_Character;
      Index : Backward_Index;
   end record;
   pragma Pack (Wide_Range);

   type Wide_Ranges is array (Range_Index range <>) of Wide_Range;
   pragma Pack (Wide_Ranges);

   function Decode
     (Char     : Character;
      Forward  : Forward_Map) return Wide_Character;

   function Encode
     (Char     : Wide_Character;
      Ranges   : Wide_Ranges;
      Backward : Backward_Map) return Character;

   pragma Inline (Decode);
   pragma Inline (Encode);

   procedure Decode
     (Text        : in     Raw_String;
      Text_Last   :    out Natural;
      Result      :    out Wide_String;
      Result_Last :    out Natural;
      Forward     : in     Forward_Map);

   procedure Encode
     (Text        : in     Wide_String;
      Text_Last   :    out Natural;
      Result      :    out Raw_String;
      Result_Last :    out Natural;
      Ranges      : in     Wide_Ranges;
      Backward    : in     Backward_Map);

end Encodings.Maps;


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
