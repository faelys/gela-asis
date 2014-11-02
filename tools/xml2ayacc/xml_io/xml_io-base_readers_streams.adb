------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


package body XML_IO.Base_Readers_Streams is
   use Implementation;

   ---------------------
   -- Attribute_Count --
   ---------------------

   function Attribute_Count (Parser : in Reader) return List_Count is
   begin
      return Attribute_Count (Implementation.Reader (Parser));
   end Attribute_Count;

   --------------------
   -- Attribute_Name --
   --------------------

   function Attribute_Name
     (Parser : in Reader;
      Index  : in List_Index)
      return XML_String
   is
   begin
      return Value (Parser.Buffer, Attribute_Name (Parser, Index));
   end Attribute_Name;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Parser : in Reader;
      Index  : in List_Index)
      return XML_String
   is
   begin
      return Value (Parser.Buffer, Attribute_Value (Parser, Index));
   end Attribute_Value;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Parser      : in Reader;
      Name        : in XML_String;
      Default     : in XML_String := Implementation.Nil_Literal;
      Raise_Error : in Boolean := False)
      return XML_String
   is
      use type XML_String;
   begin
      for J in 1 .. Attribute_Count (Parser) loop
         if Attribute_Name (Parser, J) = Name then
            return Attribute_Value (Parser, J);
         end if;
      end loop;

      if Raise_Error then
         raise Constraint_Error;
      else
         return Default;
      end if;
   end Attribute_Value;

   --------------
   -- Encoding --
   --------------

   function Encoding (Parser : in Reader) return XML_String is
   begin
      return Value (Parser.Buffer, Encoding (Parser));
   end Encoding;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Parser : in out Reader) is
   begin
      Initialize (Parser.Buffer, Parser);
   end Initialize;

   -----------------
   -- More_Pieces --
   -----------------

   function More_Pieces (Parser : in Reader) return Boolean is
   begin
      return More_Pieces (Implementation.Reader (Parser));
   end More_Pieces;

   ----------
   -- Name --
   ----------

   function Name (Parser : in Reader) return XML_String is
   begin
      return Value (Parser.Buffer, Name (Parser));
   end Name;

   ----------
   -- Next --
   ----------

   procedure Next (Parser : in out Reader) is
   begin
      Next (Parser.Buffer, Parser);
   end Next;

   ----------------
   -- Piece_Kind --
   ----------------

   function Piece_Kind (Parser : in Reader) return Piece_Kinds is
   begin
      return Piece_Kind (Implementation.Reader (Parser));
   end Piece_Kind;

   ----------
   -- Read --
   ----------

   procedure Read
     (Parser : in out Reader;
      Buffer : in out XML_String;
      Last   :    out Natural)
   is
   begin
      Read (Parser.Stream, Buffer, Last);
   end Read;

   ----------------
   -- Standalone --
   ----------------

   function Standalone (Parser : in Reader) return Boolean is
   begin
      return Standalone (Implementation.Reader (Parser));
   end Standalone;

   ----------
   -- Text --
   ----------

   function Text (Parser : in Reader) return XML_String is
   begin
      return Value (Parser.Buffer, Text (Parser));
   end Text;

end XML_IO.Base_Readers_Streams;



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
