------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


with Encodings;

generic
   type XML_Character is (<>);
   type XML_String    is array (Positive range <>) of XML_Character;

   Nil_String : in XML_String;

package XML_IO.Base_Readers is

   ------------
   -- Reader --
   ------------

   type Reader is abstract tagged limited null record;

   function More_Pieces (Parser : in Reader) return Boolean     is abstract;
   function Piece_Kind  (Parser : in Reader) return Piece_Kinds is abstract;

   procedure Next (Parser : in out Reader)
      is abstract;

   function Encoding    (Parser : in Reader) return XML_String  is abstract;
   function Encoding    (Parser : in Reader) return Encodings.Encoding
      is abstract;
   --  Piece: Start_Document

   function Standalone  (Parser : in Reader) return Boolean     is abstract;
   --  Piece: Start_Document

   function Text        (Parser : in Reader) return XML_String is abstract;
   --  Piece: Characters, Processing_Instruction, Comment

   function Name        (Parser : in Reader) return XML_String  is abstract;
   --  Piece: Start_Element, End_Element, Processing_Instruction

   function Attribute_Count (Parser : in Reader) return List_Count
      is abstract;
   --  Piece: Start_Element

   function Attribute_Name
     (Parser : in Reader;
      Index  : in List_Index) return XML_String
      is abstract;
   --  Piece: Start_Element

   function Attribute_Value
     (Parser : in Reader;
      Index  : in List_Index) return XML_String
      is abstract;
   --  Piece: Start_Element

   function Attribute_Value
     (Parser      : in Reader;
      Name        : in XML_String;
      Default     : in XML_String := Nil_String;
      Raise_Error : in Boolean := False) return XML_String
      is abstract;
   --  Piece: Start_Element

end XML_IO.Base_Readers;


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
