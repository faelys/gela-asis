------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


with Tokens;
with Generate;
with Gramar_Items;
with Nodes.Read;
with Nodes.Database;
with Gramar_Items.Code;
with Ada.Text_IO;

package body Xml_To_Y is
   procedure Run
     (ASIS_File   : String;
      Tokens_File : String;
      Syntax_File : String;
      Code_File   : String;
      Output      : String := "") is
   begin
      Nodes.Read.Read_File (ASIS_File);
      Tokens.Read_File (Tokens_File);
      Gramar_Items.Read_File (Syntax_File);
      Gramar_Items.Code.Read_Code_File (Code_File);

      if Output /= "" then
         Generate.Open_File (Output);
      end if;

      Generate.All_Tokens;
      Generate.Start_Rule;
      Generate.Token_Rules;
      Generate.All_Rules;
      Generate.Options_And_Lists;

      if Output /= "" then
         Generate.Close_File;
      end if;

      declare
         use Ada.Text_IO;
         use Nodes.Database;
         Output : File_Type;
      begin
         Create (Output, Name => "output.txt");
         for I in 1 .. Last_Node loop
            Put_Line (Output, Node_Name (I) & ":");
            for J in 1 .. Last_Attribute (I) loop
               Put_Line (Output, "   " & Attribute_Name (I, J) &
                           " : " & Attribute_Type (I, J) & ";");
            end loop;
         end loop;
         Close (Output);
      end;
   end Run;
end Xml_To_Y;


------------------------------------------------------------------------------
--  Copyright (c) 2006, Maxim Reznik
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
