--------------------------------------------------------
--                E n c o d i n g s                   --
--                                                    --
-- Tools for convertion strings between Unicode and   --
-- national/vendor character sets.                    --
--                - - - - - - - - -                   --
-- Read copyright and license at the end of this file --
--------------------------------------------------------

package body Encodings.Classes is

   function Get_Class
     (Object : in Coder;
      C      : in Wide_Character)
     return Character_Class;
   pragma Inline (Get_Class);

   ------------
   -- Decode --
   ------------

   procedure Decode
     (Text        : in     Raw_String;
      Text_Last   :    out Natural;
      Result      :    out Wide_String;
      Result_Last :    out Natural;
      Classes     :    out Character_Classes;
      Object      : in out Coder)
   is
      Index : Cache_Index;
      X     : Positive;
   begin
      Decode (Text, Text_Last, Result, Result_Last, Object.Map);

      if Object.Map = UTF_8 then
         for J in Result'First .. Result_Last loop
            Index := Wide_Character'Pos (Result (J)) mod Cache_Index'Last + 1;

            if Object.Wide (Index) = Result (J) and
              Object.Cache (Index) /= Unknown
            then
               Classes (J) := Object.Cache (Index);
            else
               Classes (J)          := Get_Class (Object, Result (J));
               Object.Cache (Index) := Classes (J);
               Object.Wide (Index)  := Result (J);

               if Classes (J) = Unknown then
                  Object.Prefix := Result (J);
               else
                  Object.Prefix := ' ';
               end if;
            end if;
         end loop;
      else
         X := Result'First;

         for J in Text'First .. Text_Last loop
            if Object.Classes (Text (J)) /= Unknown then
               Classes (X)               := Object.Classes (Text (J));
            else
               Classes (X)               := Get_Class (Object, Result (X));
               Object.Classes (Text (J)) := Classes (X);

               if Classes (X) = Unknown then
                  Object.Prefix := Result (X);
               else
                  Object.Prefix := ' ';
               end if;
            end if;

            X := X + 1;
         end loop;
      end if;
   end Decode;

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class
     (Object : in Coder;
      C      : in Wide_Character) return Character_Class is
   begin
      if Object.Prefix = ' ' then
         return Get_Class ((1 => C));
      else
         return Get_Class (Object.Prefix & C);
      end if;
   end Get_Class;

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
