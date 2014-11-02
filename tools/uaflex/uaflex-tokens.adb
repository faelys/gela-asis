------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


with Ada.Strings.Unbounded;

package body UAFlex.Tokens is

   package U renames Ada.Strings.Unbounded;

   type Node;
   type Node_Access is access all Node;
   type Node is record
      Next  : Node_Access;
      Name  : U.Unbounded_String;
      Index : Positive;
   end record;

   List : Node_Access;

   -----------
   -- Count --
   -----------

   function Count return Natural is
   begin
      if List = null then
         return 0;
      else
         return List.Index;
      end if;
   end Count;

   ---------------
   -- Get_Token --
   ---------------

   procedure Get_Token
     (Name   : in     String;
      Result :    out Positive)
   is
      use type U.Unbounded_String;
      Ptr : Node_Access := List;
   begin
      while Ptr /= null loop
         if Ptr.Name = Name then
            Result := Ptr.Index;
            return;
         end if;

         Ptr := Ptr.Next;
      end loop;

      Result := Count + 1;
      List   := new Node'(List, U.To_Unbounded_String (Name), Result);
   end Get_Token;

   ----------------
   -- Token_Name --
   ----------------

   function Token_Name (Index : Positive) return String is
      Ptr : Node_Access := List;
   begin
      while Ptr /= null loop
         if Ptr.Index = Index then
            return U.To_String (Ptr.Name);
         end if;

         Ptr := Ptr.Next;
      end loop;

      return "";
   end Token_Name;

end UAFlex.Tokens;



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
