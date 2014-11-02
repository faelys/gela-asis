------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


with UAFlex.Tokens;
with Ada.Text_IO;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;

package body Automatons.Output is
   use Symbols;
   package U renames Ada.Strings.Unbounded;

   function Get_Symbol_Classes (A : in DFA) return Symbol_Set_Array;
   function Get_Symbol_Sets    (A : in DFA) return Symbol_Set_Array;
   function Count_Symbol_Sets  (A : in DFA) return Natural;

   -----------------------
   -- Count_Symbol_Sets --
   -----------------------

   function Count_Symbol_Sets (A : in DFA) return Natural is
      Result : Natural := 0;
   begin
      for S in 1 .. Last_State (A) loop
         Result := Result + Edges (A, S);
      end loop;

      return Result;
   end Count_Symbol_Sets;

   ------------------------
   -- Get_Symbol_Classes --
   ------------------------

   function Get_Symbol_Classes (A : in DFA) return Symbol_Set_Array is
      Data  : constant Symbol_Set_Array := Get_Symbol_Sets (A);
   begin
      return Distinct_Symbol_Sets (Data);
   end Get_Symbol_Classes;

   ---------------------
   -- Get_Symbol_Sets --
   ---------------------

   function Get_Symbol_Sets (A : in DFA) return Symbol_Set_Array is
      Result : Symbol_Set_Array (1 .. Count_Symbol_Sets (A));
      Last   : Natural := 0;
   begin
      for S in 1 .. Last_State (A) loop
         for J in 1 .. Edges (A, S) loop
            declare
               Sym   : Symbol_Set := Edge_Symbols (A, S, J);
               Found : Boolean := False;
            begin
               for I in 1 .. Last loop
                  if Is_Equal (Sym, Result (I)) then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Last := Last + 1;
                  Result (Last) := Sym;
               end if;
            end;
         end loop;
      end loop;

      return Result (1 .. Last);
   end Get_Symbol_Sets;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (A     : in DFA;
      Pkg   : in String;
      Start : in Start_Names;
      Dir   : in String)
   is
      Classes   : constant Symbol_Set_Array := Get_Symbol_Classes (A);
      Surrogate : constant Positive := Classes'Length + 1;
      
      Output : Ada.Text_IO.File_Type;
      
      procedure P (X : String) is
      begin
         Ada.Text_IO.Put_Line (Output, X);
      end P;
      
      procedure N (X : String) is
      begin
         Ada.Text_IO.Put (Output, X);
      end N;

      package T renames UAFlex.Tokens;

      function Img (X : Integer) return String is
         Image : constant String := Integer'Image (X);
      begin
         return Image (2 .. Image'Last);
      end Img;

      function Image (S : State) return String is
      begin
         return Img (Integer (S - 1));
      end Image;
      
      function File_Name return String is
         use Ada.Strings.Maps;
         use Ada.Strings.Fixed;
         Up   : constant Character_Sequence := "ABCDEFGHIJKLMNOPQRSTUVWXYZ.";
         Down : constant Character_Sequence := "abcdefghijklmnopqrstuvwxyz-";
         Map : constant Character_Mapping := To_Mapping (Up, Down);
      begin
         return Translate (Pkg, Map);
      end File_Name;

      procedure Write_Tokens is
      begin
         P ("   type Token is");

         for I in 1 .. T.Count loop
            if I = 1 then
               N ("     (" & T.Token_Name (I));
            else
               P (",");
               N ("      " & T.Token_Name (I));
            end if;
         end loop;
         P (");");
      end Write_Tokens;

      Last  : constant State := Last_State (A) + 1;
      Count : Natural;
      EOF   : Positive;
      Error : Positive;
      Tkn   : Token;

      Start_State : Natural;
   begin
      T.Get_Token ("End_Of_Input", EOF);
      T.Get_Token ("Error", Error);
      
      Ada.Text_IO.Put_Line ("Writing " & Dir & "/" & File_Name & ".ads");
      Ada.Text_IO.Create (Output, Name => Dir & "/" & File_Name & ".ads");
      P ("with Gela.Classificators;");
      P ("with Gela.Character_Class_Buffers; use Gela;");
      P ("with Asis.Gela.Parser.Tokens;");

      P ("");
      P ("package " & Pkg & " is");
      -- Write_Tokens;
      P ("   subtype Token is Asis.Gela.Parser.Tokens.Token;");
      P ("   Error : constant Token := Asis.Gela.Parser.Tokens.Error;");
      P ("");

      --  0 - end_of_buffer
      --  Classes'Length + 1 - surrogate
      --  Classes'Length + 2 - unknown character

      P ("   subtype Character_Class is " &
         "Character_Class_Buffers.Character_Class");
      P ("     range 0 .. " & Img (Classes'Length + 2) & ";");
      P ("");
      P ("   type State is mod "
         & Image (Last + 1) & ";");
      P ("");

      for S in 1 .. Last_State (A) loop
         Start_State := Get_Start (A, S);

         if Start_State > 0 then
            P ("   " & U.To_String (U.Head (Start (Start_State), 20))
               & " : constant State := " & Image (S) & ";");
         end if;
      end loop;
      P ("");
      P ("   function Switch (S : State; C : Character_Class) return State;");
      P ("   pragma Inline (Switch);");
      P ("");
      P ("   function Accepted (S : State) return Token;");
      P ("   pragma Inline (Accepted);");
      P ("");

      P ("");
      P ("   subtype Code_Point is Classificators.Code_Point;");
      P ("");
      P ("   function Get_Class (Pos : Code_Point) return Character_Class;");
      P ("");
      P ("end " & Pkg & ";");
      Ada.Text_IO.Close (Output);
      Ada.Text_IO.Put_Line ("Writing " & Dir & "/" & File_Name & ".adb");
      Ada.Text_IO.Create (Output, Name => Dir & "/" & File_Name & ".adb");
      P ("");
      P ("-- This file is auto generated by uaflex. Don't edit");
      P ("package body " & Pkg & " is");
      P ("   use Asis.Gela.Parser.Tokens;");
      P ("");
      P ("   type Switch_Table is array "
         & "(State range <>, Character_Class range <>) of State;");
      P ("   type Accepted_Table is array (State range <>) of Token;");
      P ("");
      P ("   Table  : constant Switch_Table (0 .. " & Image (Last - 1)
         & ", 1 .. " & Img (Classes'Length + 1) & ") := ");

      for S in 1 .. Last_State (A) loop
         if S = 1 then
            P ("     (" & Image (S) & " =>");
         else
            P ("      " & Image (S) & " =>");
         end if;

         Count := 0;

         for J in 1 .. Edges (A, S) loop
            declare
               Sym   : Symbol_Set := Edge_Symbols (A, S, J);
               Jump  : State := Edge_Jump (A, S, J);
            begin
               for C in Classes'Range loop
                  if Classes (C) * Sym then
                     if Count = 0 then
                        N ("        (");
                     elsif Count mod 6 = 0 then
                        P (",");
                        N ("         ");
                     else
                        N (", ");
                     end if;
                     N (Img (C) & " => " & Image (Jump));
                     Count := Count + 1;
                  end if;
               end loop;
            end;
         end loop;

         if Count = Classes'Length then
            N (", " & Img (Surrogate) & " => " & Image (S) & ")");
         elsif Count = 0 then
            N ("        (" & Img (Surrogate) & " => " & Image (S) &
               ", others => " & Image (Last) & ")");
         else
            N (", " & Img (Surrogate) & " => " & Image (S) &
               ", others => " & Image (Last) & ")");
         end if;

         if S = Last - 1 then
            P (");");
         else
            P (",");
         end if;
      end loop;

      P ("");
      P ("   Finish : constant Accepted_Table :=");
      for S in 1 .. Last_State (A) loop
         if S = 1 then
            N ("     (");
         end if;

         Tkn := Get_Token (A, S);
         if Tkn = 0 then
            N (T.Token_Name (Error));
         else
            N (T.Token_Name (Positive (Tkn)));
         end if;

         if S = Last - 1 then
            P (");");
         else
            N (",");
            if S mod 6 = 0 then
               P ("");
               N ("      ");
            end if;
         end if;
      end loop;
      P ("");
      P ("   ---------------");
      P ("   -- Get_Class --");
      P ("   ---------------");
      P ("");
      P ("   function Get_Class (Pos : Code_Point) return Character_Class is");
      P ("   begin");
      P ("      case Pos is");
      for C in Classes'Range loop
         P (Range_Image (Classes (C), "         ") & " =>");
         P ("            return " & Img (C) & ";");
      end loop;
      P ("         when others =>");
      P ("            return " & Img (Classes'Last + 2) & ";");
      P ("      end case;");
      P ("   end Get_Class;");
      P ("");
      P ("   ------------");
      P ("   -- Switch --");
      P ("   ------------");
      P ("");
      P ("   function Switch (S : State; C : Character_Class) " &
         "return State is");
      P ("   begin");
      P ("      return Table (S, C);");
      P ("   end Switch;");
      P ("");
      P ("   --------------");
      P ("   -- Accepted --");
      P ("   --------------");
      P ("");
      P ("   function Accepted (S : State) return Token is");
      P ("   begin");
      P ("      return Finish (S);");
      P ("   end Accepted;");
      P ("");
      P ("end " & Pkg & ";");
      Ada.Text_IO.Close (Output);
   end Generate;

end Automatons.Output;


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
