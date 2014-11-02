------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $
--  Purpose:

generic
   type Item_Type is private;
   type Index_Type is range <>;
package Gela.Containers.Vectors is
   pragma Preelaborate;

   type Vector is private;

   procedure Add
     (Object : in out Vector;
      Item   : in     Item_Type);

   procedure Clear (Object : in out Vector);

   procedure Free (Object : in out Vector);

   function Length (Object : Vector) return Index_Type'Base;

   function Get (Object : Vector; Index : Index_Type) return Item_Type;

   procedure Set
     (Object : in out Vector;
      Index  : in     Index_Type;
      Item   : in     Item_Type);

   procedure Copy
     (Target : in out Vector;
      Source : in     Vector);

private
   type Vector_Node;
   type Vector is access all Vector_Node;

   Default_Size : constant Index_Type := 8 * 4096 / Item_Type'Size;

   type Table is array (Index_Type range <>) of Item_Type;

   type Vector_Node (Size : Index_Type) is record
      Data : Table (1 .. Size);
      Last : Index_Type'Base := 0;
      Next : Vector;
   end record;

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
