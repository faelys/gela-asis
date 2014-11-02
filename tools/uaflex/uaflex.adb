------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Macros;
with UAFlex.Tokens;
with Automatons;
with Automatons.Output;
with Automatons.Minimize;
with Regular_Expressions.Create;

package body UAFlex is

   procedure Error (Msg, Text : String := "") is
   begin
      Put_Line (Standard_Error, Msg & Text);
   end Error;

   procedure Run
     (Input_File : String;
      Pkg_Name   : String;
      Output_Dir : String)
   is
      use Ada.Strings;
      use Regular_Expressions;
      package U renames Ada.Strings.Unbounded;
      use type U.Unbounded_String;

      Max_Starts : constant := 30;
      Top        : array (1 .. Max_Starts) of Expression;
      Top_Set    : array (1 .. Max_Starts) of Boolean := (others => False);
      Exclusive  : array (1 .. Max_Starts) of Boolean := (others => False);
      Start      : Automatons.Output.Start_Names (1 .. Max_Starts);
      Last_Start : Positive := 1;

      procedure New_Rule (Index : Natural; Name : String; Expr : Expression) is
         Token : Positive;
         Rule  : Expression;
      begin
         Tokens.Get_Token (Name, Token);
         Rule := New_Rule (Expr, Token);

         if Top_Set (Index) then
            Top (Index) := New_Alternative (Top (Index), Rule);
         else
            Top (Index) := Rule;
            Top_Set (Index) := True;
         end if;
      end New_Rule;

      procedure New_Rule (Prefix : String; Name : String; Expr : Expression) is
         Comma     : constant Maps.Character_Set := Maps.To_Set (',');
         Word_From : Positive := Prefix'First;
         Word_To   : Natural;
         Found     : Boolean := False;
      begin
         loop
            Fixed.Find_Token (Source => Prefix (Word_From .. Prefix'Last),
                              Set    => Comma,
                              Test   => Outside,
                              First  => Word_From,
                              Last   => Word_To);

            exit when Word_To = 0;

            Found := False;

            for I in 1 .. Last_Start loop
               if Start (I) = Prefix (Word_From .. Word_To)
               then
                  Found := True;
                  New_Rule (I, Name, Expr);
               end if;
            end loop;

            if not Found then
               Error ("Not a state: ", Prefix (Word_From .. Word_To));
            end if;

            Word_From := Word_To + 1;
            Found     := True;
         end loop;

         if not Found then  --  no prefix <...> exists
            for I in 1 .. Last_Start loop
               if not Exclusive (I) then
                  New_Rule (I, Name, Expr);
               end if;
            end loop;
         end if;
      end New_Rule;

      procedure New_Macro (Name : String; Expr : Expression) is
      begin
         Macros.Add (Name, Expr);
      end New_Macro;

      type Sections is (Macro_Section, Rule_Section);

      Input     : File_Type;
      Line      : String (1 .. 4000);
      Last      : Natural;
      Section   : Sections := Macro_Section;
      Spaces    : constant Maps.Character_Set := Maps.To_Set (' ' & ASCII.HT);
      Word_From : Positive;
      Word_To   : Natural;
      Rule_From : Positive;
      Rule_To   : Natural;
      Pref_From : Positive;
      Pref_To   : Natural;
      Expr      : Expression;
      DFA       : Automatons.DFA;
   begin
      Open (Input, In_File, Input_File);

      while not End_Of_File (Input) loop
         Get_Line (Input, Line, Last);

         if Last = 0 then
            null;
         elsif Last < 2 then
            Error ("Bad line:", Line (1 .. Last));
            return;
         elsif Line (1 .. Last) = "%%" then
            Section := Sections'Succ (Section);
         elsif Line (1 .. 2) = "--" then
            null;
         elsif Section = Macro_Section and Line (1) = '%'
           and (Line (2) = 's' or Line (2) = 'x') then
            Fixed.Find_Token (Source => Line (3 .. Last),
                              Set    => Spaces,
                              Test   => Outside,
                              First  => Word_From,
                              Last   => Word_To);
            if Word_To = 0 then
               Error ("Bad line: ", Line (1 .. Last));
               return;
            end if;

            Last_Start := Last_Start + 1;
            Start (Last_Start) :=
              U.To_Unbounded_String (Line (Word_From .. Word_To));
            Exclusive (Last_Start) := Line (2) = 'x';
         else
            Fixed.Find_Token (Source => Line (1 .. Last),
                              Set    => Spaces,
                              Test   => Outside,
                              First  => Word_From,
                              Last   => Word_To);

            if Word_To = 0 or Word_To = Last then
               Error ("Bad line:", Line (1 .. Last));
               return;
            end if;

            if Line (Word_From) = '<'
              and Line (Word_To) = '>'
              and Section = Rule_Section
            then
               Pref_From := Word_From + 1;
               Pref_To   := Word_To - 1;

               Fixed.Find_Token (Source => Line (Word_To + 1 .. Last),
                                 Set    => Spaces,
                                 Test   => Outside,
                                 First  => Word_From,
                                 Last   => Word_To);

               if Word_To = 0 or Word_To = Last then
                  Error ("Bad line:", Line (1 .. Last));
                  return;
               end if;
            else
               Pref_From := 1;
               Pref_To   := 0;
            end if;

            Fixed.Find_Token (Source => Line (Word_To + 1 .. Last),
                              Set    => Spaces,
                              Test   => Outside,
                              First  => Rule_From,
                              Last   => Rule_To);

            if Rule_To = 0 then
               Error ("Bad line:", Line (1 .. Last));
               return;
            end if;

            begin
               Expr := Create (Line (Rule_From .. Last));
            exception
               when Syntax_Error =>
                  Error ("Bad rule:", Line (Rule_From .. Last));
            end;

            if Section = Macro_Section then
               New_Macro (Line (Word_From .. Word_To), Expr);
            else
               New_Rule (Line (Pref_From .. Pref_To),
                         Line (Word_From .. Word_To),
                         Expr);
            end if;
         end if;
      end loop;

      for I in 1 .. Last_Start loop
         if not Top_Set (I) then
            Error ("No rule found for state " & U.To_String (Start (I)));
            return;
         end if;

         Add_To_DFA (DFA, Top (I), I);
      end loop;

      Start (1) := U.To_Unbounded_String ("Default");
      Automatons.Minimize (DFA);
      Automatons.Output.Generate (DFA, Pkg_Name, Start, Output_Dir);
   end Run;
   
end UAFlex;


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
