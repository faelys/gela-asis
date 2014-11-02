------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


with Ada.Unchecked_Deallocation;

package body Positions is

   ---------
   -- "+" --
   ---------

   function "+" (Object : in Iterator) return Position is
   begin
      return Object.Data;
   end "+";

   ---------
   -- Add --
   ---------

   procedure Add
     (Object : in out Set;
      Pos    : in     Position)
   is
      X : Set := Object;
   begin
      if Object = null then
         Object := new Node'(Pos, null);
      elsif Object.Data < Pos then
         Object := new Node'(Pos, Object);
      elsif Object.Data > Pos then
         Add (Object.Next, Pos);
      end if;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Object : in out Set;
      Pos    : in     Set)
   is
      Element : Iterator := Start (Pos);
   begin
      while Exist (Element) loop
         Add (Object, +Element);
         Element := Next (Element);
      end loop;
   end Add;

   -----------
   -- Clear --
   -----------

   procedure Clear (Object : in out Set) is
      procedure Free is new Ada.Unchecked_Deallocation (Node, Set);
   begin
      if Object /= null then
         Clear (Object.Next);
         Free (Object);
      end if;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Object : in Set;
      Pos    : in Position)
      return Boolean
   is
      Element : Iterator := Start (Object);
   begin
      while Exist (Element) loop
         if Pos = +Element then
            return True;
         end if;

         Element := Next (Element);
      end loop;

      return False;
   end Contains;

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : Set) return Boolean is
   begin
      if Left = null and Right = null then
         return True;
      elsif Left /= null and Right /= null then
         return Left.Data = Right.Data and then
           Equal (Left.Next, Right.Next);
      else
         return False;
      end if;
   end Equal;

   -----------
   -- Exist --
   -----------

   function Exist (Object : in Iterator) return Boolean is
   begin
      return Object /= null;
   end Exist;

   ----------
   -- Next --
   ----------

   function Next (Object : in Iterator) return Iterator is
   begin
      return Iterator (Object.Next);
   end Next;

   -----------
   -- Start --
   -----------

   function Start (Object : in Set) return Iterator is
   begin
      return Iterator (Object);
   end Start;

end Positions;



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
