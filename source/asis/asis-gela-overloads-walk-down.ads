------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $
--  Purpose:
--  Helper procedures to implement up-to-down pass

with Asis.Gela.Classes;

package Asis.Gela.Overloads.Walk.Down is

   procedure Function_Call
     (Resolver : in out Down_Resolver;
      Element  : in out Asis.Element);

   procedure Explicit_Dereference
     (Resolver : in out Down_Resolver;
      Element  : in     Asis.Element);

   procedure Selected_Component
     (Resolver : in out Down_Resolver;
      Element  : in     Asis.Element);

   procedure Attribute_Reference
     (Resolver : in out Down_Resolver;
      Element  : in     Asis.Element);

   procedure Aggregate
     (Resolver  : in out Down_Resolver;
      Element   : in out Asis.Element;
      Extension : in     Boolean := False);

   procedure Membership
     (Resolver : in out Down_Resolver;
      Element  : in out Asis.Element);

   procedure Qualified_Expression
     (Resolver : in out Down_Resolver;
      Element  : in out Asis.Element);

   procedure Check_Implicit
     (Resolver : in out Down_Resolver;
      Element  : in out Asis.Element;
      Control  : in out Traverse_Control);

   procedure Assignment
     (Resolver : in out Down_Resolver;
      Element  : in out Asis.Element);

   procedure Set_Expression_Type
     (Element : Asis.Expression;
      Tipe    : Classes.Type_Info);

   procedure Set_Expression_Type
     (Element : Asis.Expression;
      Tipe    : Asis.Element);

end Asis.Gela.Overloads.Walk.Down;


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
