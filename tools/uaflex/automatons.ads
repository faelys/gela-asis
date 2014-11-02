------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


with Symbols;
with Positions;

package Automatons is

   type State is private;
   type Token is new Natural;

   Not_A_Token : constant Token := 0;

   type DFA is private;

   procedure Need_State
     (Automaton : in out DFA;
      S         :    out State;
      Set       : in out Positions.Set;
      Start     : in     Natural := 0);

   procedure Add_Edge
     (Automaton : in out DFA;
      From      : in     State;
      To        : in     State;
      Set       : in     Symbols.Symbol_Set);

   function Has_More (Automaton : in DFA) return Boolean;

   procedure Get_Next
     (Automaton : in out DFA;
      S         :    out State;
      Set       :    out Positions.Set);

   procedure Set_Token
     (Automaton : in out DFA;
      S         : in     State;
      Target    : in     Token);

   procedure Clear_All_Sets (Automaton : in out DFA);

   procedure Delete (Automaton : in out DFA);

private

   type State is new Natural;

   type Edge;
   type Edge_Access is access all Edge;

   type Edge is record
      Next   : aliased Edge_Access;
      Jump   : State;
      Symbol : Symbols.Symbol_Set;
   end record;

   type State_Info is record
      Set    : Positions.Set;
      Edge   : aliased Edge_Access;
      Target : Token   := Not_A_Token;
      Start  : Natural := 0;
   end record;

   Length : constant State := 1024;
   type State_Array is array (State range 1 .. Length) of State_Info;

   type Node;
   type DFA is access all Node;

   type Node is record
      First  : State;
      Last   : State;
      Marked : State;
      Next   : DFA;
      States : State_Array;
   end record;

   -- Child access methods

   function Last_State (Automaton : DFA) return State;

   function Get_Token
     (Automaton : DFA;
      S         : State) return Token;

   function Get_Start
     (Automaton : DFA;
      S         : State) return Natural;

   function Edges
     (Automaton : DFA;
      S         : State) return Natural;

   function Edge_Symbols
     (Automaton : DFA;
      S         : State;
      Edge      : Positive) return Symbols.Symbol_Set;

   function Edge_Jump
     (Automaton : DFA;
      S         : State;
      Edge      : Positive) return State;

   procedure Create_State
     (Automaton : in out DFA;
      S         :    out State;
      Set       : in     Positions.Set := Positions.Empty_Set;
      Start     : in     Natural := 0);

end Automatons;


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
