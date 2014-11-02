------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $:

with Gela.Source_Buffers;             use Gela;
with Gela.Source_Buffers.Current;

with Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with Asis.Gela.Compilations;
with Asis.Gela.Contexts.Utils;

package body Asis.Gela.Text_Utils is

   type Buffer_Access is access all Source_Buffers.Current.Source_Buffer;

   ----------------------------
   -- Compilation_Line_Count --
   ----------------------------

   function Compilation_Line_Count
     (Unit : Asis.Compilation_Unit) return Asis.Asis_Natural
   is
      List : constant Compilations.Compilation_List :=
        Contexts.Utils.Compilation_List (Enclosing_Context (Unit.all));
      Comp : constant Compilations.Compilation :=
        Compilations.Enclosing (Unit);
   begin
      return Compilations.Get_Line_Count (List, Comp);
   end Compilation_Line_Count;

   -------------
   -- Decoder --
   -------------

   function Decoder (Unit : Asis.Compilation_Unit) return Decoder_Access is
      List : constant Compilations.Compilation_List :=
        Contexts.Utils.Compilation_List (Enclosing_Context (Unit.all));
      Comp : constant Compilations.Compilation :=
        Compilations.Enclosing (Unit);
   begin
      return Compilations.Decoder (List, Comp);
   end Decoder;

   ----------
   -- Free --
   ----------

   procedure Free (Buffer : in out Source_Buffer_Access) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Source_Buffers.Source_Buffer'Class, Source_Buffer_Access);
   begin
      Destroy (Buffer);
   end Free;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Unit  : Asis.Compilation_Unit;
      Index : Asis.Asis_Positive) return Lines.Line
   is
      List : constant Compilations.Compilation_List :=
        Contexts.Utils.Compilation_List (Enclosing_Context (Unit.all));
      Comp : constant Compilations.Compilation :=
        Compilations.Enclosing (Unit);
   begin
      return Compilations.Get_Line (List, Comp, Index);
   end Get_Line;

   ----------------
   -- New_Buffer --
   ----------------

   function New_Buffer (File : in Wide_String) return Source_Buffer_Access is
      use Ada.Characters.Handling;

      Result : constant Buffer_Access :=
        new Source_Buffers.Current.Source_Buffer;
      Name   : constant String := To_String (File);
   begin
      Source_Buffers.Current.Open (Result.all, Name);

      return Source_Buffer_Access (Result);
   end New_Buffer;

   ----------------
   -- Get_Buffer --
   ----------------

   function Source_Buffer
     (Unit : Asis.Compilation_Unit)
     return Source_Buffer_Access
   is
      List : constant Compilations.Compilation_List :=
        Contexts.Utils.Compilation_List (Enclosing_Context (Unit.all));
      Comp : constant Compilations.Compilation :=
        Compilations.Enclosing (Unit);
   begin
      return Compilations.Source_Buffer (List, Comp);
   end Source_Buffer;

end Asis.Gela.Text_Utils;


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
