--------------------------------------------------------
--                E n c o d i n g s                   --
--                                                    --
-- Tools for convertion strings between Unicode and   --
-- national/vendor character sets.                    --
--                - - - - - - - - -                   --
-- Read copyright and license at the end of this file --
--------------------------------------------------------

with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Encodings.Maps.Linked;

package body Encodings is

   ------------
   -- Decode --
   ------------

   function Decode
     (Text : Raw_String;
      Map  : Encoding)
      return Wide_String
   is
      Text_Last   : Natural;
      Result_Last : Natural;
      Result      : Wide_String (Text'Range);
   begin
      Decode (Text, Text_Last, Result, Result_Last, Map);

      if Text_Last /= Text'Last then
         raise Invalid_Encoding;
      end if;

      return Result (Result'First .. Result_Last);
   end Decode;

   ------------
   -- Decode --
   ------------

   procedure Decode
     (Text        : in     Raw_String;
      Text_Last   :    out Natural;
      Result      :    out Wide_String;
      Result_Last :    out Natural;
      Map         : in     Encoding)
   is
   begin
      if Map = Unknown then
         raise Invalid_Encoding;
      elsif Decoder_List (Map) = null then
         if Decoder_List (Unknown) = null then
            raise Invalid_Encoding;
         else
            Decoder_List (Unknown).all
              (Text, Text_Last, Result, Result_Last, Map);
         end if;
      else
         Decoder_List (Map).all (Text, Text_Last, Result, Result_Last, Map);
      end if;
   end Decode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Text : Wide_String;
      Map  : Encoding)
      return Raw_String
   is
      Text_Last   : Natural;
      Result_Last : Natural;
      Result      : Raw_String (1 .. 3 * Text'Length);
   begin
      Encode (Text, Text_Last, Result, Result_Last, Map);

      if Text_Last /= Text'Last then
         raise Invalid_Encoding;
      end if;

      return Result (Result'First .. Result_Last);
   end Encode;

   ------------
   -- Encode --
   ------------

   procedure Encode
     (Text        : in     Wide_String;
      Text_Last   :    out Natural;
      Result      :    out Raw_String;
      Result_Last :    out Natural;
      Map         : in     Encoding)
   is
   begin
      if Map = Unknown then
         raise Invalid_Encoding;
      elsif Encoder_List (Map) = null then
         if Encoder_List (Unknown) = null then
            raise Invalid_Encoding;
         else
            Encoder_List (Unknown).all
              (Text, Text_Last, Result, Result_Last, Map);
         end if;
      else
         Encoder_List (Map).all (Text, Text_Last, Result, Result_Last, Map);
      end if;
   end Encode;

   -----------------
   -- To_Encoding --
   -----------------

   Dash_To_Underscore : constant Ada.Strings.Maps.Character_Mapping :=
     Ada.Strings.Maps.To_Mapping ("-", "_");

   function To_Encoding (Name : String) return Encoding is
      use Ada.Strings.Fixed;
      use Ada.Characters.Handling;

      function Fix (Name : String) return String is
      begin
         if Index (Name, "WINDOWS") = Name'First then
            return Replace_Slice (Name, Name'First, Name'First + 6, "CP");
         else
            return Name;
         end if;
      end Fix;

      Upper  : constant String :=
        Fix (Translate (To_Upper (Name), Dash_To_Underscore));
      Result : Encoding;
   begin
      Result := Encoding'Value (Upper);
      return Result;
   exception
      when Constraint_Error =>
         raise Invalid_Encoding;
   end To_Encoding;

end Encodings;



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
