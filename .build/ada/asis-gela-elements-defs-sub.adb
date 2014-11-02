
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

package body Asis.Gela.Elements.Defs.Sub is

   function Get_Subtype_Mark
     (Element : S_Discrete_Subtype_Indication_Node) return Asis.Expression is
   begin
      return Element.Subtype_Mark;
   end Get_Subtype_Mark;

   procedure Set_Subtype_Mark
     (Element : in out S_Discrete_Subtype_Indication_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Subtype_Mark := Value;
   end Set_Subtype_Mark;

   function Subtype_Constraint
     (Element : S_Discrete_Subtype_Indication_Node) return Asis.Constraint is
   begin
      return Element.Subtype_Constraint;
   end Subtype_Constraint;

   procedure Set_Subtype_Constraint
     (Element : in out S_Discrete_Subtype_Indication_Node;
      Value   : in     Asis.Constraint) is
   begin
      Element.Subtype_Constraint := Value;
   end Set_Subtype_Constraint;

   function New_S_Discrete_Subtype_Indication_Node
     (The_Context : ASIS.Context)
      return S_Discrete_Subtype_Indication_Ptr
   is
      Result : S_Discrete_Subtype_Indication_Ptr :=
       new S_Discrete_Subtype_Indication_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_S_Discrete_Subtype_Indication_Node;
  
   function Discrete_Range_Kind (Element : S_Discrete_Subtype_Indication_Node)
      return Asis.Discrete_Range_Kinds is
   begin
      return A_Discrete_Subtype_Indication;
   end;

   function Children (Element : access S_Discrete_Subtype_Indication_Node)
     return Traverse_List is
   begin
      return ((False, Element.Subtype_Mark'Access),
        (False, Element.Subtype_Constraint'Access));
   end Children;

   function Clone
     (Element : S_Discrete_Subtype_Indication_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant S_Discrete_Subtype_Indication_Ptr := new S_Discrete_Subtype_Indication_Node;
   begin
      Result.Enclosing_Element := Parent;
      Result.Is_Part_Of_Implicit := Element.Is_Part_Of_Implicit;
      Result.Is_Part_Of_Inherited := Element.Is_Part_Of_Inherited;
      Result.Is_Part_Of_Instance := Element.Is_Part_Of_Instance;
      Result.Start_Position := Element.Start_Position;
      Result.End_Position := Element.End_Position;
      Result.Enclosing_Compilation_Unit :=
        Enclosing_Compilation_Unit (Parent.all);
      Result.Hash := Element.Hash;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access S_Discrete_Subtype_Indication_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Subtype_Mark :=
        Copy (Cloner, Get_Subtype_Mark (Source.all), Asis.Element (Target));
      Target.Subtype_Constraint :=
        Copy (Cloner, Subtype_Constraint (Source.all), Asis.Element (Target));
   end Copy;

   function Range_Attribute
     (Element : S_Discrete_Range_Attribute_Reference_Node) return Asis.Expression is
   begin
      return Element.Range_Attribute;
   end Range_Attribute;

   procedure Set_Range_Attribute
     (Element : in out S_Discrete_Range_Attribute_Reference_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Range_Attribute := Value;
   end Set_Range_Attribute;

   function New_S_Discrete_Range_Attribute_Reference_Node
     (The_Context : ASIS.Context)
      return S_Discrete_Range_Attribute_Reference_Ptr
   is
      Result : S_Discrete_Range_Attribute_Reference_Ptr :=
       new S_Discrete_Range_Attribute_Reference_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_S_Discrete_Range_Attribute_Reference_Node;
  
   function Discrete_Range_Kind (Element : S_Discrete_Range_Attribute_Reference_Node)
      return Asis.Discrete_Range_Kinds is
   begin
      return A_Discrete_Range_Attribute_Reference;
   end;

   function Children (Element : access S_Discrete_Range_Attribute_Reference_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Range_Attribute'Access));
   end Children;

   function Clone
     (Element : S_Discrete_Range_Attribute_Reference_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant S_Discrete_Range_Attribute_Reference_Ptr := new S_Discrete_Range_Attribute_Reference_Node;
   begin
      Result.Enclosing_Element := Parent;
      Result.Is_Part_Of_Implicit := Element.Is_Part_Of_Implicit;
      Result.Is_Part_Of_Inherited := Element.Is_Part_Of_Inherited;
      Result.Is_Part_Of_Instance := Element.Is_Part_Of_Instance;
      Result.Start_Position := Element.Start_Position;
      Result.End_Position := Element.End_Position;
      Result.Enclosing_Compilation_Unit :=
        Enclosing_Compilation_Unit (Parent.all);
      Result.Hash := Element.Hash;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access S_Discrete_Range_Attribute_Reference_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Range_Attribute :=
        Copy (Cloner, Range_Attribute (Source.all), Asis.Element (Target));
   end Copy;

   function Lower_Bound
     (Element : S_Discrete_Simple_Expression_Range_Node) return Asis.Expression is
   begin
      return Element.Lower_Bound;
   end Lower_Bound;

   procedure Set_Lower_Bound
     (Element : in out S_Discrete_Simple_Expression_Range_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Lower_Bound := Value;
   end Set_Lower_Bound;

   function Upper_Bound
     (Element : S_Discrete_Simple_Expression_Range_Node) return Asis.Expression is
   begin
      return Element.Upper_Bound;
   end Upper_Bound;

   procedure Set_Upper_Bound
     (Element : in out S_Discrete_Simple_Expression_Range_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Upper_Bound := Value;
   end Set_Upper_Bound;

   function New_S_Discrete_Simple_Expression_Range_Node
     (The_Context : ASIS.Context)
      return S_Discrete_Simple_Expression_Range_Ptr
   is
      Result : S_Discrete_Simple_Expression_Range_Ptr :=
       new S_Discrete_Simple_Expression_Range_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_S_Discrete_Simple_Expression_Range_Node;
  
   function Discrete_Range_Kind (Element : S_Discrete_Simple_Expression_Range_Node)
      return Asis.Discrete_Range_Kinds is
   begin
      return A_Discrete_Simple_Expression_Range;
   end;

   function Children (Element : access S_Discrete_Simple_Expression_Range_Node)
     return Traverse_List is
   begin
      return ((False, Element.Lower_Bound'Access),
        (False, Element.Upper_Bound'Access));
   end Children;

   function Clone
     (Element : S_Discrete_Simple_Expression_Range_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant S_Discrete_Simple_Expression_Range_Ptr := new S_Discrete_Simple_Expression_Range_Node;
   begin
      Result.Enclosing_Element := Parent;
      Result.Is_Part_Of_Implicit := Element.Is_Part_Of_Implicit;
      Result.Is_Part_Of_Inherited := Element.Is_Part_Of_Inherited;
      Result.Is_Part_Of_Instance := Element.Is_Part_Of_Instance;
      Result.Start_Position := Element.Start_Position;
      Result.End_Position := Element.End_Position;
      Result.Enclosing_Compilation_Unit :=
        Enclosing_Compilation_Unit (Parent.all);
      Result.Hash := Element.Hash;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access S_Discrete_Simple_Expression_Range_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Lower_Bound :=
        Copy (Cloner, Lower_Bound (Source.all), Asis.Element (Target));
      Target.Upper_Bound :=
        Copy (Cloner, Upper_Bound (Source.all), Asis.Element (Target));
   end Copy;

end Asis.Gela.Elements.Defs.Sub;
