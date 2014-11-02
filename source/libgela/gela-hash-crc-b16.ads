------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  Purpose:

--  Name  : CRC-16 [CCITT]
--  Width : 16
--  Poly  : 0x1021 x^16 + x^12 + x^5 + 1
--  Init  : 0xFFFF
--  RefIn : False
--  RefOut: False
--  XorOut: 0x0000
--  Check : 0x29B1 ("123456789")
--  MaxLen: 4095 byte (32767 bit)

with Ada.Streams;
with Interfaces;

package Gela.Hash.CRC.b16 is

   type CRC16 is new Interfaces.Unsigned_16;

   type Hasher is private;

   Initial_Hasher : constant Hasher;

   procedure Update
     (This  : in out Hasher;
      Value : in     String);

   procedure Wide_Update
     (This  : in out Hasher;
      Value : in     Wide_String);

   procedure Wide_Wide_Update
     (This  : in out Hasher;
      Value : in     Wide_Wide_String);

   procedure Update
     (This  : in out Hasher;
      Value : in     Ada.Streams.Stream_Element_Array);

   function Result
     (This : in Hasher)
      return CRC16;

   function Calculate
     (Value : in String)
      return CRC16;

   function Wide_Calculate
     (Value : in Wide_String)
      return CRC16;

   function Wide_Wide_Calculate
     (Value : in Wide_Wide_String)
      return CRC16;

   function Calculate
     (Value : in Ada.Streams.Stream_Element_Array)
      return CRC16;

   function To_Hash
     (T : in CRC16)
      return Hash_Type;
   pragma Inline (To_Hash);

   function Calculate
     (Value : in String)
      return Hash_Type;
   pragma Inline (Calculate);

   function Wide_Calculate
     (Value : in Wide_String)
      return Hash_Type;
   pragma Inline (Calculate);

   function Wide_Wide_Calculate
     (Value : in Wide_Wide_String)
      return Hash_Type;
   pragma Inline (Calculate);

   function Calculate
     (Value : in Ada.Streams.Stream_Element_Array)
      return Hash_Type;
   pragma Inline (Calculate);

private

   type Hasher is record
      Length : Integer  := 0;
      Cm_Reg : CRC16 := 16#FFFF#;
   end record;

   Initial_Hasher : constant Hasher :=
     (Length => 0, Cm_Reg => 16#FFFF#);

end Gela.Hash.CRC.b16;

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
