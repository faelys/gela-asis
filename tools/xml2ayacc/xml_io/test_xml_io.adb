------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


with Ada.Text_IO;
with Text_Streams.File;
with XML_IO.Stream_Readers;

procedure Test_XML_IO is
   use XML_IO;
   package R renames XML_IO.Stream_Readers;

   Stream : aliased Text_Streams.File.File_Text_Stream;
   Parser : R.Reader (Stream'Access, 32);-- R.Default_Buffer_Size);
begin
   Text_Streams.File.Open (Stream, "aaa.xml");
   R.Initialize (Parser);

   while R.More_Pieces (Parser) loop
      Ada.Text_IO.Put_Line
        ("Kind: " & Piece_Kinds'Image (R.Piece_Kind (Parser)));

      case R.Piece_Kind (Parser) is
         when Start_Document =>
            Ada.Text_IO.Put_Line
              (" Encoding: " & R.Encoding (Parser));
            Ada.Text_IO.Put_Line
              (" Standalone: " & Boolean'Image (R.Standalone (Parser)));

         when DTD | End_Document =>
            null;

         when Entity_Reference =>
            Ada.Text_IO.Put_Line
              (" Name: " & R.Name (Parser));

         when Start_Element =>
            Ada.Text_IO.Put_Line
              (" Name: " & R.Name (Parser));

            for I in 1 .. R.Attribute_Count (Parser) loop
               Ada.Text_IO.Put_Line
                 (" Attr : " & R.Attribute_Name (Parser, I)
                  & " = " & R.Attribute_Value (Parser, I));
            end loop;

         when End_Element =>
            Ada.Text_IO.Put_Line
              (" Name: " & R.Name (Parser));

         when Comment
           | Characters
           | CDATA_Section
           =>
            Ada.Text_IO.Put_Line
              (" Text: " & R.Text (Parser));

         when Processing_Instruction =>
            Ada.Text_IO.Put_Line
              (" Name: " & R.Name (Parser));
            Ada.Text_IO.Put_Line
              (" Text: " & R.Text (Parser));

         when Attribute | Namespace =>
            null;
      end case;

      R.Next (Parser);
   end loop;
end Test_XML_IO;


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
