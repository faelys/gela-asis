------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  Purpose:
--  Dictionary for set keyword's ID, with automatic clean unused IDs.

with Ada.Finalization;
with Gela.Hash.CRC.b16;

package Gela.Repository.Dictionary is

   type ID is private;

--     subtype Code_Point is Wide_Wide_Character range
--       Wide_Wide_Character'Val (0) .. Wide_Wide_Character'Val (16#10FFFF#);

   subtype Code_Point_Array is Wide_Wide_String;

   type Gela_Dictionary is abstract
     new Ada.Finalization.Limited_Controlled with private;

   procedure Get_ID
     (This   : in out Gela_Dictionary;
      Value  : in     Code_Point_Array;
      Result :    out ID);

   function Get_Name
     (This  : in Gela_Dictionary;
      Value : in ID)
      return Code_Point_Array;

   procedure Marck
     (This  : in Gela_Dictionary;
      Value : in ID);

   procedure Clear
     (This : in out Gela_Dictionary);

   function Count
     (This : in Gela_Dictionary)
      return Natural;

   Use_Error : exception;

private

   type ID is new Gela.Hash.CRC.b16.CRC16;

   type Code_Point_Array_Access is
     access all Code_Point_Array;

   type ID_Point is record
      Num  : ID := 0;
      Data : Code_Point_Array_Access := null;
      Used : Boolean := False;
   end record;

   type ID_Point_Array is
     array (Positive range <>) of ID_Point;

   type ID_Point_Array_Access is
     access all ID_Point_Array;

   type Gela_Dictionary is abstract
     new Ada.Finalization.Limited_Controlled
   with record
      Data    : ID_Point_Array_Access := null;
      Changed : Boolean := False;
   end record;

   procedure Finalize
     (This : in out Gela_Dictionary);

   function Find
     (This : in Gela_Dictionary;
      Num  : in    ID)
      return Natural;

   procedure Insert
     (This  : in out Gela_Dictionary;
      Index : in     Positive;
      Point : in     ID_Point);

   procedure Delete
     (This  : in out Gela_Dictionary;
      Index : in     Positive);

   procedure Free_Unused
     (This : in out Gela_Dictionary);

   procedure Redirect_Save
     (This : in out Gela_Dictionary'Class);

   procedure Save
     (This : in out Gela_Dictionary); -- is null;

end Gela.Repository.Dictionary;

------------------------------------------------------------------------------
--  Copyright (c) 2006, Andry Ogorodnik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--     * this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--     * notice, this list of conditions and the following disclaimer in the
--     * documentation and/or other materials provided with the distribution.
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
