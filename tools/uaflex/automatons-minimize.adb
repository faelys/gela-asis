------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


procedure Automatons.Minimize (A : in out DFA) is

   Last         : constant State := Last_State (A);
   Error_State  : constant State := Last + 1;
   Second_State : constant State := 2;

   type Equive_Array is array (1 .. Error_State) of State;
   Equive       : Equive_Array := (others => 1);
   Next_Equive  : Equive_Array := (others => 1);

   function Check_Equive_Class (X, Y : State) return Boolean is
      use Symbols;
   begin
      for I in 1 .. Edges (A, X) loop
         declare
            Sym_X  : Symbol_Set := Edge_Symbols (A, X, I);
            Jump_X : constant State := Edge_Jump (A, X, I);
         begin
            for J in 1 .. Edges (A, Y) loop
               declare
                  Sym_Y  : Symbol_Set := Edge_Symbols (A, Y, J);
                  Jump_Y : constant State := Edge_Jump (A, Y, J);
               begin
                  if Sym_X * Sym_Y then
                     if Equive (Jump_X) /= Equive (Jump_Y) then
                        return False;
                     else
                        Sym_X := Sym_X - Sym_Y;
                     end if;
                  end if;
               end;
            end loop;

            if not Is_Empty (Sym_X)
              and Equive (Jump_X) /= Equive (Error_State)
            then
               return False;
            end if;
         end;
      end loop;

      return True;
   end Check_Equive_Class;

   Current_Equive_Class : State;
   Prev_Equive_Class    : State := Second_State;
   Eqv_State            : State;
   Result               : DFA;
   Found                : Boolean;
   Starting             : Natural;
   Max_Starting         : Natural;

begin
   Init_Equive_Classes :
      for I in 1 .. Last loop
         if Get_Token (A, I) /= Not_A_Token then
            Equive (I) := Second_State;
         end if;
      end loop Init_Equive_Classes;

   Try_Split_Equive_Classes :
      loop
         Current_Equive_Class := 0;

         Set_Equive_Classes :
            for I in 1 .. Last loop
               Found := False;

               Find_Existent_Class :
                  for J in 1 .. I - 1 loop
                     if Equive (I) = Equive (J)
                       and then Get_Token (A, I) = Get_Token (A, J)
                     then
                        Found := Check_Equive_Class (I, J)
                          and then Check_Equive_Class (J, I);

                        if Found then
                           Next_Equive (I) := Next_Equive (J);
                           exit Find_Existent_Class;
                        end if;
                     end if;
                  end loop Find_Existent_Class;

               if not Found then
                  Current_Equive_Class := Current_Equive_Class + 1;
                  Next_Equive (I) := Current_Equive_Class;
               end if;
            end loop Set_Equive_Classes;

            Current_Equive_Class := Current_Equive_Class + 1;
            Next_Equive (Error_State) := Current_Equive_Class;

         exit Try_Split_Equive_Classes
            when Prev_Equive_Class = Current_Equive_Class;

         Prev_Equive_Class := Current_Equive_Class;
         Equive := Next_Equive;
      end loop Try_Split_Equive_Classes;

   --  Create_DFA

   for I in 1 .. Current_Equive_Class - 1 loop
      Max_Starting := 0;

      for J in Equive'Range loop
         if Equive (J) = I then
            Starting := Get_Start (A, J);
            if Max_Starting < Starting then
               Max_Starting := Starting;
            end if;
         end if;
      end loop;

      Create_State (Result, Eqv_State, Start => Max_Starting);
   end loop;

   for I in 1 .. Last loop
      for J in 1 .. Edges (A, I) loop
         Add_Edge (Result,
                   From => Equive (I),
                   To   => Equive (Edge_Jump (A, I, J)),
                   Set  => Edge_Symbols (A, I, J));
      end loop;

      Set_Token (Result, Equive (I), Get_Token (A, I));
   end loop;

   Delete (A);
   A := Result;
end Automatons.Minimize;


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
