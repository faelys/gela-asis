------------------------------------------------------------------------------
--                           G E L A   X A S I S                            --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $
--  Purpose:
--  Generic implementation of static expression calculator

with Asis;
with XASIS.Classes;

generic
   type Value is private;

   type Static_Range is array (Bound_Kinds) of Value;

   type Calculator is limited private;

   with function Literal
     (Object  : access Calculator;
      Element : in     Asis.Expression) return Value is <>;

   with function Operator
     (Object  : access Calculator;
      Tipe    : in     Classes.Type_Info;
      Kind    : in     Asis.Operator_Kinds;
      Args    : in     Asis.Association_List) return Value is <>;

   with function Attribute
     (Object  : access Calculator;
      Tipe    : in     Classes.Type_Info;
      Kind    : in     Asis.Attribute_Kinds;
      Element : in     Asis.Expression) return Value is <>;

   with function Attribute_Call
     (Object  : access Calculator;
      Tipe    : in     Classes.Type_Info;
      Kind    : in     Asis.Attribute_Kinds;
      Args    : in     Asis.Association_List) return Value is <>;

   with function Check_Range
     (Object   : access Calculator;
      Expr     : in     Asis.Expression;
      Bounds   : in     Static_Range;
      Invert   : in     Boolean) return Value is <>;

   with function Undefined
     (Object  : access Calculator;
      Element : in     Asis.Expression) return Value is <>;

   with function Range_Of_Array
     (Object  : access Calculator;
      Decl    : in     Asis.Declaration;
      Attr    : in     Asis.Expression) return Static_Range is <>;

   with function Range_Of_Type
     (Object   : access Calculator;
      Type_Def : in     Asis.Definition) return Static_Range is <>;

   with function String_Constant_Range
     (Object  : access Calculator;
      Decl    : in     Asis.Declaration) return Static_Range is <>;

package XASIS.Static.Iter is

   function Evaluate
     (Object  : access Calculator;
      Element : in     Asis.Expression) return Value;

   --  Implimentation related functions

   function Constrained_Array_Range
     (Object  : access Calculator;
      Decl    : in     Asis.Declaration;
      Index   : in     Asis.ASIS_Positive)
     return Static_Range;

   function Evaluate_Static_Constant
     (Object  : access Calculator;
      Element : in     Asis.Declaration)
     return Value;

   function Get_Discrete_Range
     (Object  : access Calculator;
      Element : in     Asis.Definition) return Static_Range;

   function Static_Range_Attribute   --  like Is_Static_Bound
     (Object : access Calculator;
      Attr   : in     Asis.Expression) return Static_Range;

end XASIS.Static.Iter;


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
