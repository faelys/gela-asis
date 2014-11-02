--------------------------------------------------------
--                E n c o d i n g s                   --
--                                                    --
-- Tools for convertion strings between Unicode and   --
-- national/vendor character sets.                    --
--                - - - - - - - - -                   --
-- Read copyright and license at the end of this file --
--------------------------------------------------------

package body Encodings.Maps.UTF_8 is

   ------------
   -- Decode --
   ------------

   procedure Decode
     (Text        : in     Raw_String;
      Text_Last   :    out Natural;
      Result      :    out Wide_String;
      Result_Last :    out Natural;
      Map         : in     Encoding := Encodings.UTF_8)
   is
      Index  : Positive := Text'First;
      Value  : Natural;
      T_Last : Natural := Text'First - 1;
      R_Last : Natural := Result'First - 1;
   begin
      for I in Result'Range loop
         exit when Index > Text'Last;

         Value := Character'Pos (Text (Index));

         case Value is
            when 16#E0# .. 16#ff# =>
               exit when Index > Text'Last - 2;
               Value := (Value - 16#E0#) * 2 ** 6;
               Index := Index + 1;
               Value := Value + Character'Pos (Text (Index)) - 16#80#;
               Value := Value * 2 ** 6;
               Index := Index + 1;
               Value := Value + Character'Pos (Text (Index)) - 16#80#;

            when 16#C0# .. 16#DF# =>
               exit when Index > Text'Last - 1;
               Value := (Value - 16#C0#) * 2 ** 6;
               Index := Index + 1;
               Value := Value + Character'Pos (Text (Index)) - 16#80#;

            when others =>
               null;
         end case;

         Result (I) := Wide_Character'Val (Value);
         T_Last := Index;
         R_Last := I;
         Index := Index + 1;
      end loop;

      Text_Last := T_Last;
      Result_Last := R_Last;
   end Decode;

   ------------
   -- Encode --
   ------------

   procedure Encode
     (Text        : in     Wide_String;
      Text_Last   :    out Natural;
      Result      :    out Raw_String;
      Result_Last :    out Natural;
      Map         : in     Encoding := Encodings.UTF_8)
   is
      Index  : Positive := Result'First;
      Value  : Natural;
      T_Last : Natural := Text'First - 1;
      R_Last : Natural := Result'First - 1;
   begin
      for I in Text'Range loop
         exit when Index > Result'Last;

         Value := Wide_Character'Pos (Text (I));

         case Value is
            when 16#0000# .. 16#007f# =>
               Result (Index) := Character'Val (Value);

            when 16#0080# .. 16#07ff# =>
               exit when Index > Result'Last - 1;

               Result (Index) := Character'Val (16#C0# + Value / 2 ** 6);
               Index := Index + 1;
               Result (Index) := Character'Val (16#80# + Value mod 2 ** 6);

            when others =>
               exit when Index > Result'Last - 2;

               Result (Index) := Character'Val (16#E0# + Value / 2 ** 12);
               Index := Index + 1;
               Result (Index) :=
                 Character'Val (16#80# + Value mod 2 ** 12 / 2 ** 6);
               Index := Index + 1;
               Result (Index) := Character'Val (16#80# + Value mod 2 ** 6);
         end case;

         T_Last := I;
         R_Last := Index;
         Index  := Index + 1;
      end loop;

      Text_Last := T_Last;
      Result_Last := R_Last;
   end Encode;

begin
   Encoder_List (Encodings.UTF_8) := Encode'Access;
   Decoder_List (Encodings.UTF_8) := Decode'Access;
end Encodings.Maps.UTF_8;


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
