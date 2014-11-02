------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


with Text_Streams;
with XML_IO.Internals;
with XML_IO.Base_Readers;

generic
   with package Implementation is new XML_IO.Internals.Implementation (<>);

   type Input_Stream (Input : access Text_Streams.Text_Stream'Class) is
     limited private;

   with procedure Read
     (Input  : in out Input_Stream;
      Buffer : in out Implementation.XML_String;
      Free   :    out Natural);

package XML_IO.Base_Readers_Streams is

   package Base_Readers renames Implementation.Readers;
   subtype XML_String   is Implementation.XML_String;

   type Reader (Input       : access Text_Streams.Text_Stream'Class;
                Buffer_Size : Positive) is
     new Base_Readers.Reader with private;

   function More_Pieces     (Parser : in Reader) return Boolean;
   function Piece_Kind      (Parser : in Reader) return Piece_Kinds;
   function Encoding        (Parser : in Reader) return XML_String;
   function Standalone      (Parser : in Reader) return Boolean;
   function Text            (Parser : in Reader) return XML_String;
   function Name            (Parser : in Reader) return XML_String;
   function Attribute_Count (Parser : in Reader) return List_Count;

   procedure Initialize     (Parser : in out Reader);
   procedure Next (Parser : in out Reader);


   function Attribute_Name
     (Parser : in Reader;
      Index  : in List_Index) return XML_String;

   function Attribute_Value
     (Parser : in Reader;
      Index  : in List_Index) return XML_String;

   function Attribute_Value
     (Parser      : in Reader;
      Name        : in XML_String;
      Default     : in XML_String := Implementation.Nil_Literal;
      Raise_Error : in Boolean := False) return XML_String;

   Default_Buffer_Size : constant Positive := 8192;

private

   type Reader (Input       : access Text_Streams.Text_Stream'Class;
                Buffer_Size : Positive) is
     new Implementation.Reader with record
        Stream : Input_Stream (Input);
        Buffer : Xml_String (1 .. Buffer_Size);
     end record;

   procedure Read
     (Parser : in out Reader;
      Buffer : in out XML_String;
      Last   :    out Natural);

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
