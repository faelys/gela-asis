------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $:

package body Asis.Gela.Scanners is

   -----------
   -- Enter --
   -----------

   procedure Enter
     (Object : in out Scanner;
      State  : in     Scanner_Tables.State) is
   begin
      Object.Start := State;
   end Enter;

   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token
     (Object : in out Scanner;
      Token  :    out Gela.Scanner_Tables.Token)
   is
      use type Scanner_Tables.State;
      use type Scanner_Tables.Token;
      use type Character_Class_Buffers.Character_Class;

      Error         : constant Scanner_Tables.State :=
        Scanner_Tables.State'Last;
      Surrogate     : constant Character_Class_Buffers.Character_Class :=
        Character_Class_Buffers.Character_Class'Last - 1;
      Current_State : Scanner_Tables.State := Object.Start;
      Result        : Scanner_Tables.Token := Scanner_Tables.Error;
      Class         : Character_Class_Buffers.Character_Class;
      Position      : Source_Buffers.Cursor := Object.To;
      Accepted      : Scanner_Tables.Token;
      Sur_Count     : Natural := 0;
   begin
      Object.From := Position;

      loop
         Character_Class_Buffers.Get (Object.Classes, Class);

         if Class = Character_Class_Buffers.End_Of_Buffer then
            Classificators.Read
              (Object.Classificator.all, Object.Input, Object.Classes);
         else
            Current_State := Scanner_Tables.Switch (Current_State, Class);

            exit when Current_State = Error;

            Source_Buffers.Next (Position);

            Accepted := Scanner_Tables.Accepted (Current_State);

            Sur_Count := Sur_Count + Boolean'Pos (Class = Surrogate);

            if Accepted /= Scanner_Tables.Error then
               Result := Accepted;
               Character_Class_Buffers.Mark (Object.Classes);
               Object.To := Position;
               Object.Surrogates := Sur_Count;
            end if;
         end if;
      end loop;

      Character_Class_Buffers.Back_To_Mark (Object.Classes);

      Token := Result;
   end Next_Token;

   ------------------
   -- Token_Length --
   ------------------

   function Token_Length (Object : Scanner) return Positive is
      use type Source_Buffers.Cursor;
   begin
      return Object.To - Object.From - Object.Surrogates;
   end Token_Length;

   ----------------
   -- Token_Span --
   ----------------

   procedure Token_Span
     (Object : in     Scanner;
      From   :    out Source_Buffers.Cursor;
      To     :    out Source_Buffers.Cursor)
   is
   begin
      From := Object.From;
      To   := Object.To;
   end Token_Span;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Object :    out Scanner;
      Cursor : in     Source_Buffers.Cursor)
   is
   begin
      Object.Start := Scanner_Tables.Default;
      Object.Input := Cursor;
      Object.From  := Cursor;
      Object.To    := Cursor;
   end Initialize;

end Asis.Gela.Scanners;

------------------------------------------------------------------------------
--  Copyright (c) 2008-2013, Maxim Reznik
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
