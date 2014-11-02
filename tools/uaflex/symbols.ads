------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


with Ada.Finalization;

package Symbols is

   type Symbol is mod 16#110001#;

   type Symbol_Array is array (Positive range <>) of Symbol;

   type Symbol_Set is private;

   function To_Range  (Single       : Symbol)       return Symbol_Set;
   function To_Range  (Sequence     : Symbol_Array) return Symbol_Set;
   function To_Range  (Lower, Upper : Symbol)       return Symbol_Set;
   function "or"      (Left, Right  : Symbol_Set)   return Symbol_Set;
   function "and"     (Left, Right  : Symbol_Set)   return Symbol_Set;
   function "-"       (Left, Right  : Symbol_Set)   return Symbol_Set;
   function "*"       (Left, Right  : Symbol_Set)   return Boolean;
   function Intersect (Left, Right  : Symbol_Set)   return Boolean renames "*";
   function Is_Empty  (Left         : Symbol_Set)   return Boolean;
   function Is_Equal  (Left, Right  : Symbol_Set)   return Boolean;

   function Range_Image
     (Left   : Symbol_Set;
      Indent : String) return String;

   Empty        : constant Symbol_Set;
   Not_A_Symbol : constant Symbol := Symbol'Last;

   type Symbol_Set_Array is array (Positive range <>) of Symbol_Set;

   function Distinct_Symbol_Sets
     (Next : in Symbol_Set_Array) return Symbol_Set_Array;

private

   package F renames Ada.Finalization;

   type Set_Node;
   type Node_Access is access all Set_Node;

   type Symbol_Set is new F.Controlled with record
      Node : Node_Access;
   end record;

   type Symbol_Range is record
      Lower, Upper : Symbol;
   end record;

   function "*"      (Left, Right  : Symbol_Range) return Boolean;
   function "or"     (Left, Right  : Symbol_Range) return Symbol_Range;
   function "and"    (Left, Right  : Symbol_Range) return Symbol_Range;

   type Symbol_Ranges is array (Positive range <>) of Symbol_Range;

   type Set_Node (Length : Natural) is record
      Count  : Natural;
      Ranges : Symbol_Ranges (1 .. Length);
   end record;

   procedure Adjust (Object : in out Symbol_Set);
   procedure Finalize (Object : in out Symbol_Set);

   Empty_Node : aliased Set_Node :=
     (0, 2, (others => (Not_A_Symbol, Not_A_Symbol)));
   Empty : constant Symbol_Set := (F.Controlled with Empty_Node'Access);

   pragma Inline ("*");
   pragma Inline (Is_Empty);
   pragma Inline (Is_Equal);

end Symbols;


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
