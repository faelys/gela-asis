------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

with Ada.Unchecked_Deallocation;

package body Gela.Containers.Vectors is

   ---------
   -- Add --
   ---------

   procedure Add
     (Object : in out Vector;
      Item   : in     Item_Type)
   is
   begin
      if Object = null then
         Object := new Vector_Node (Default_Size);
      end if;

      if Object.Last < Object.Size then
         Object.Last := Object.Last + 1;
         Object.Data (Object.Last) := Item;
      else
         Add (Object.Next, Item);
      end if;
   end Add;

   -----------
   -- Clear --
   -----------

   procedure Clear (Object : in out Vector) is
   begin
      if Object /= null then
         Object.Last := 0;
         Clear (Object.Next);
      end if;
   end Clear;

   ----------
   -- Free --
   ----------

   procedure Free (Object : in out Vector) is
      procedure Release is new
        Ada.Unchecked_Deallocation (Vector_Node, Vector);
   begin
      if Object /= null then
         Free (Object.Next);
         Release (Object);
      end if;
   end Free;

   ------------
   -- Length --
   ------------

   function Length (Object : Vector) return Index_Type'Base is
   begin
      if Object = null then
         return 0;
      else
         return Length (Object.Next) + Object.Last;
      end if;
   end Length;

   ---------
   -- Get --
   ---------

   function Get (Object : Vector; Index : Index_Type) return Item_Type is
   begin
      if Object = null then
         raise Constraint_Error;
      elsif Index <= Object.Last then
         return Object.Data (Index);
      else
         return Get (Object.Next, Index - Object.Last);
      end if;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (Object : in out Vector;
      Index  : in     Index_Type;
      Item   : in     Item_Type) is
   begin
      if Object = null then
         raise Constraint_Error;
      elsif Index <= Object.Last then
         Object.Data (Index) := Item;
      else
         Set (Object.Next, Index - Object.Last, Item);
      end if;
   end Set;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Target : in out Vector;
      Source : in     Vector)
   is
      Temp : Vector := Source;
      Pos  : Index_Type := 1;
   begin
      if Target /= null and then Target.Size /= Length (Source) then
         Free  (Target);
      end if;

      if Source /= null and Target = null then
         Target := new Vector_Node (Length (Source));
         Target.Last := Target.Size;
      else
         Clear (Target);
      end if;

      while Temp /= null loop
         Target.Data (Pos .. Pos + Temp.Last - 1) :=
           Temp.Data (1 .. Temp.Last);

         Pos  := Pos + Temp.Last;
         Temp := Temp.Next;
      end loop;
   end Copy;

end Gela.Containers.Vectors;


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
