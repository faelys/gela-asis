
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

package body Asis.Gela.Elements.Defs.Const is

   function Range_Attribute
     (Element : Range_Attribute_Reference_Node) return Asis.Expression is
   begin
      return Element.Range_Attribute;
   end Range_Attribute;

   procedure Set_Range_Attribute
     (Element : in out Range_Attribute_Reference_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Range_Attribute := Value;
   end Set_Range_Attribute;

   function New_Range_Attribute_Reference_Node
     (The_Context : ASIS.Context)
      return Range_Attribute_Reference_Ptr
   is
      Result : Range_Attribute_Reference_Ptr :=
       new Range_Attribute_Reference_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Range_Attribute_Reference_Node;
  
   function Constraint_Kind (Element : Range_Attribute_Reference_Node)
      return Asis.Constraint_Kinds is
   begin
      return A_Range_Attribute_Reference;
   end;

   function Children (Element : access Range_Attribute_Reference_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Range_Attribute'Access));
   end Children;

   function Clone
     (Element : Range_Attribute_Reference_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Range_Attribute_Reference_Ptr := new Range_Attribute_Reference_Node;
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
      Target : access Range_Attribute_Reference_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Range_Attribute :=
        Copy (Cloner, Range_Attribute (Source.all), Asis.Element (Target));
   end Copy;

   function Lower_Bound
     (Element : Simple_Expression_Range_Node) return Asis.Expression is
   begin
      return Element.Lower_Bound;
   end Lower_Bound;

   procedure Set_Lower_Bound
     (Element : in out Simple_Expression_Range_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Lower_Bound := Value;
   end Set_Lower_Bound;

   function Upper_Bound
     (Element : Simple_Expression_Range_Node) return Asis.Expression is
   begin
      return Element.Upper_Bound;
   end Upper_Bound;

   procedure Set_Upper_Bound
     (Element : in out Simple_Expression_Range_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Upper_Bound := Value;
   end Set_Upper_Bound;

   function New_Simple_Expression_Range_Node
     (The_Context : ASIS.Context)
      return Simple_Expression_Range_Ptr
   is
      Result : Simple_Expression_Range_Ptr :=
       new Simple_Expression_Range_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Simple_Expression_Range_Node;
  
   function Constraint_Kind (Element : Simple_Expression_Range_Node)
      return Asis.Constraint_Kinds is
   begin
      return A_Simple_Expression_Range;
   end;

   function Children (Element : access Simple_Expression_Range_Node)
     return Traverse_List is
   begin
      return ((False, Element.Lower_Bound'Access),
        (False, Element.Upper_Bound'Access));
   end Children;

   function Clone
     (Element : Simple_Expression_Range_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Simple_Expression_Range_Ptr := new Simple_Expression_Range_Node;
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
      Target : access Simple_Expression_Range_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Lower_Bound :=
        Copy (Cloner, Lower_Bound (Source.all), Asis.Element (Target));
      Target.Upper_Bound :=
        Copy (Cloner, Upper_Bound (Source.all), Asis.Element (Target));
   end Copy;

   function Digits_Expression
     (Element : Digits_Constraint_Node) return Asis.Expression is
   begin
      return Element.Digits_Expression;
   end Digits_Expression;

   procedure Set_Digits_Expression
     (Element : in out Digits_Constraint_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Digits_Expression := Value;
   end Set_Digits_Expression;

   function Real_Range_Constraint
     (Element : Digits_Constraint_Node) return Asis.Range_Constraint is
   begin
      return Element.Real_Range_Constraint;
   end Real_Range_Constraint;

   procedure Set_Real_Range_Constraint
     (Element : in out Digits_Constraint_Node;
      Value   : in     Asis.Range_Constraint) is
   begin
      Element.Real_Range_Constraint := Value;
   end Set_Real_Range_Constraint;

   function New_Digits_Constraint_Node
     (The_Context : ASIS.Context)
      return Digits_Constraint_Ptr
   is
      Result : Digits_Constraint_Ptr :=
       new Digits_Constraint_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Digits_Constraint_Node;
  
   function Constraint_Kind (Element : Digits_Constraint_Node)
      return Asis.Constraint_Kinds is
   begin
      return A_Digits_Constraint;
   end;

   function Children (Element : access Digits_Constraint_Node)
     return Traverse_List is
   begin
      return ((False, Element.Digits_Expression'Access),
        (False, Element.Real_Range_Constraint'Access));
   end Children;

   function Clone
     (Element : Digits_Constraint_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Digits_Constraint_Ptr := new Digits_Constraint_Node;
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
      Target : access Digits_Constraint_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Digits_Expression :=
        Copy (Cloner, Digits_Expression (Source.all), Asis.Element (Target));
      Target.Real_Range_Constraint :=
        Copy (Cloner, Real_Range_Constraint (Source.all), Asis.Element (Target));
   end Copy;

   function Delta_Expression
     (Element : Delta_Constraint_Node) return Asis.Expression is
   begin
      return Element.Delta_Expression;
   end Delta_Expression;

   procedure Set_Delta_Expression
     (Element : in out Delta_Constraint_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Delta_Expression := Value;
   end Set_Delta_Expression;

   function Real_Range_Constraint
     (Element : Delta_Constraint_Node) return Asis.Range_Constraint is
   begin
      return Element.Real_Range_Constraint;
   end Real_Range_Constraint;

   procedure Set_Real_Range_Constraint
     (Element : in out Delta_Constraint_Node;
      Value   : in     Asis.Range_Constraint) is
   begin
      Element.Real_Range_Constraint := Value;
   end Set_Real_Range_Constraint;

   function New_Delta_Constraint_Node
     (The_Context : ASIS.Context)
      return Delta_Constraint_Ptr
   is
      Result : Delta_Constraint_Ptr :=
       new Delta_Constraint_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Delta_Constraint_Node;
  
   function Constraint_Kind (Element : Delta_Constraint_Node)
      return Asis.Constraint_Kinds is
   begin
      return A_Delta_Constraint;
   end;

   function Children (Element : access Delta_Constraint_Node)
     return Traverse_List is
   begin
      return ((False, Element.Delta_Expression'Access),
        (False, Element.Real_Range_Constraint'Access));
   end Children;

   function Clone
     (Element : Delta_Constraint_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Delta_Constraint_Ptr := new Delta_Constraint_Node;
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
      Target : access Delta_Constraint_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Delta_Expression :=
        Copy (Cloner, Delta_Expression (Source.all), Asis.Element (Target));
      Target.Real_Range_Constraint :=
        Copy (Cloner, Real_Range_Constraint (Source.all), Asis.Element (Target));
   end Copy;

   function Discrete_Ranges
     (Element : Index_Constraint_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Definition_Lists.To_Element_List
        (Element.Discrete_Ranges, Include_Pragmas);
   end Discrete_Ranges;

   procedure Set_Discrete_Ranges
     (Element : in out Index_Constraint_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Discrete_Ranges := Primary_Definition_Lists.List (Value);
   end Set_Discrete_Ranges;

   function Discrete_Ranges_List
     (Element : Index_Constraint_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Discrete_Ranges);
   end Discrete_Ranges_List;

   function New_Index_Constraint_Node
     (The_Context : ASIS.Context)
      return Index_Constraint_Ptr
   is
      Result : Index_Constraint_Ptr :=
       new Index_Constraint_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Index_Constraint_Node;
  
   function Constraint_Kind (Element : Index_Constraint_Node)
      return Asis.Constraint_Kinds is
   begin
      return An_Index_Constraint;
   end;

   function Children (Element : access Index_Constraint_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Discrete_Ranges)));
   end Children;

   function Clone
     (Element : Index_Constraint_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Index_Constraint_Ptr := new Index_Constraint_Node;
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
      Target : access Index_Constraint_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Discrete_Ranges
        (Target.all,
         Primary_Definition_Lists.Deep_Copy 
           (Discrete_Ranges (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Discriminant_Associations
     (Element : Discriminant_Constraint_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Association_Lists.To_Element_List
        (Element.Discriminant_Associations, Include_Pragmas);
   end Discriminant_Associations;

   procedure Set_Discriminant_Associations
     (Element : in out Discriminant_Constraint_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Discriminant_Associations := Primary_Association_Lists.List (Value);
   end Set_Discriminant_Associations;

   function Discriminant_Associations_List
     (Element : Discriminant_Constraint_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Discriminant_Associations);
   end Discriminant_Associations_List;

   function Normalized_Discriminant_Associations
     (Element : Discriminant_Constraint_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Association_Lists.To_Element_List
        (Element.Normalized_Discriminant_Associations, Include_Pragmas);
   end Normalized_Discriminant_Associations;

   procedure Add_To_Normalized_Discriminant_Associations
     (Element : in out Discriminant_Constraint_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Association_Lists.Add (Element.Normalized_Discriminant_Associations, Item);
   end Add_To_Normalized_Discriminant_Associations;

   function New_Discriminant_Constraint_Node
     (The_Context : ASIS.Context)
      return Discriminant_Constraint_Ptr
   is
      Result : Discriminant_Constraint_Ptr :=
       new Discriminant_Constraint_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Discriminant_Constraint_Node;
  
   function Constraint_Kind (Element : Discriminant_Constraint_Node)
      return Asis.Constraint_Kinds is
   begin
      return A_Discriminant_Constraint;
   end;

   function Children (Element : access Discriminant_Constraint_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Discriminant_Associations)));
   end Children;

   function Clone
     (Element : Discriminant_Constraint_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Discriminant_Constraint_Ptr := new Discriminant_Constraint_Node;
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
      null;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Discriminant_Constraint_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Discriminant_Associations
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Discriminant_Associations (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

end Asis.Gela.Elements.Defs.Const;
