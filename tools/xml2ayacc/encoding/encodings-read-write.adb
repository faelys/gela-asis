--------------------------------------------------------
--                E n c o d i n g s                   --
--                                                    --
-- Tools for convertion strings between Unicode and   --
-- national/vendor character sets.                    --
--                - - - - - - - - -                   --
-- Read copyright and license at the end of this file --
--------------------------------------------------------

with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Integer_Text_IO;
with Ada.Characters.Handling;

procedure Encodings.Read.Write
  (Object  : Mapping;
   Map     : Encoding)
is
   use Ada.Text_IO;
   use Encodings.Maps;

   Output : File_Type;
   Image  : constant String := Encoding'Image (Map);

   function Name return String is
      use Ada.Characters.Handling;
      Image : constant String := Encoding'Image (Map);
   begin
      return "encodings-maps-" & To_Lower (Image);
   end Name;

   procedure P (Text : String) is
   begin
      Put_Line (Output, Text);
   end P;

   function C (Char : Character) return String is
      Image : String (1 .. 6);
   begin
      Ada.Integer_Text_IO.Put (Image, Character'Pos (Char), 16);
      return "Character'Val(" & Image & ')';
   end C;

   function W (Char : Wide_Character) return String is
      Image : String (1 .. 8);
   begin
      Ada.Integer_Text_IO.Put (Image, Wide_Character'Pos (Char), 16);
      return "Wide_Character'Val(" & Image & ')';
   end W;

   function N (Int : Integer) return String is
   begin
      return Integer'Image (Int);
   end N;

begin
   Create (Output, Name => Name & ".ads");
   P ("--  Auto generated file. Don't edit");
   P ("");
   P ("package Encodings.Maps." & Image & " is");
   P ("");
   P ("   function Decode (Char : Character) return Wide_Character;");
   P ("   pragma Inline (Decode);");
   P ("");
   P ("   procedure Encode");
   P ("     (Text        : in     Wide_String;");
   P ("      Text_Last   :    out Natural;");
   P ("      Result      :    out Raw_String;");
   P ("      Result_Last :    out Natural;");
   P ("      Map         : in     Encoding := Encodings." & Image & ");");
   P ("");
   P ("   procedure Decode");
   P ("     (Text        : in     Raw_String;");
   P ("      Text_Last   :    out Natural;");
   P ("      Result      :    out Wide_String;");
   P ("      Result_Last :    out Natural;");
   P ("      Map         : in     Encoding := Encodings." & Image & ");");
   P ("");
   P ("end Encodings.Maps." & Image & ";");
   Close (Output);

   Create (Output, Name => Name & ".adb");
   P ("--  Auto generated file. Don't edit");
   P ("");
   P ("package body Encodings.Maps." & Image & " is");
   P ("");
   P ("   Forward  : Forward_Map  (" & C (Object.Start)
      & " .. Character'Last) :=");

   for I in Object.Forward'Range loop
      if I = Object.Start and I = Character'Last then
         P ("     (" & C (Object.Start)
            & " => " & W (Object.Forward (I)) & ");");
      elsif I = Object.Start then
         P ("     (" & W (Object.Forward (I)) & ",");
      elsif I = Character'Last then
         P ("      " & W (Object.Forward (I)) & ");");
      else
         P ("      " & W (Object.Forward (I)) & ",");
      end if;
   end loop;

   P ("");
   P ("   Ranges   : Maps.Wide_Ranges  (1 .. "
      & N (Object.Ranges'Length) & ") :=");

   for I in Object.Ranges'Range loop
      if I = 1 and I = Object.Ranges'Last then
         P ("     (1 => (" & W (Object.Ranges (I).Lower) & ","
            &  W (Object.Ranges (I).Upper) &  ","
            & N (Integer (Object.Ranges (I).Index)) & "));");
      elsif I = 1 then
         P ("     ((" & W (Object.Ranges (I).Lower) & ","
            &  W (Object.Ranges (I).Upper) &  ","
            & N (Integer (Object.Ranges (I).Index)) & "),");
      elsif I = Object.Ranges'Last then
         P ("      (" & W (Object.Ranges (I).Lower) & ","
            &  W (Object.Ranges (I).Upper) &  ","
            & N (Integer (Object.Ranges (I).Index)) & "));");
      else
         P ("      (" & W (Object.Ranges (I).Lower) & ","
            &  W (Object.Ranges (I).Upper) &  ","
            & N (Integer (Object.Ranges (I).Index)) & "),");
      end if;
   end loop;

   P ("");
   P ("   Backward : Maps.Backward_Map (1 .. "
      & N (Object.Backward'Length) & ") :=");

   for I in Object.Backward'Range loop
      if I = 1 then
         P ("     (" & C (Object.Backward (I)) & ",");
      elsif I = Object.Backward'Last then
         P ("      " & C (Object.Backward (I)) & ");");
      else
         P ("      " & C (Object.Backward (I)) & ",");
      end if;
   end loop;


   P ("");
   P ("   function Decode (Char : Character) return Wide_Character is");
   P ("   begin");
   P ("      return Decode (Char, Forward);");
   P ("   end Decode;");
   P ("");
   P ("   procedure Decode");
   P ("     (Text        : in     Raw_String;");
   P ("      Text_Last   :    out Natural;");
   P ("      Result      :    out Wide_String;");
   P ("      Result_Last :    out Natural;");
   P ("      Map         : in     Encoding := Encodings." & Image & ")");
   P ("   is");
   P ("   begin");
   P ("      Decode (Text, Text_Last, Result, Result_Last, Forward);");
   P ("   end Decode;");
   P ("");
   P ("   procedure Encode");
   P ("     (Text        : in     Wide_String;");
   P ("      Text_Last   :    out Natural;");
   P ("      Result      :    out Raw_String;");
   P ("      Result_Last :    out Natural;");
   P ("      Map         : in     Encoding := Encodings." & Image & ")");
   P ("   is");
   P ("   begin");
   P ("      Encode (Text, Text_Last, Result, Result_Last,");
   P ("              Ranges, Backward);");
   P ("   end Encode;");
   P ("");
   P ("begin");
   P ("   Encoder_List (Encodings." & Image & ") := Encode'Access;");
   P ("   Decoder_List (Encodings." & Image & ") := Decode'Access;");
   P ("end Encodings.Maps." & Image & ";");
   Close (Output);
end Encodings.Read.Write;



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
