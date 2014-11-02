
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
  
package Asis.Gela.Elements.Defs.Const is

   ------------------------------------
   -- Range_Attribute_Reference_Node --
   ------------------------------------

   type Range_Attribute_Reference_Node is 
      new Constraint_Node with private;

   type Range_Attribute_Reference_Ptr is
      access all Range_Attribute_Reference_Node;
   for Range_Attribute_Reference_Ptr'Storage_Pool use Lists.Pool;

   function New_Range_Attribute_Reference_Node
     (The_Context : ASIS.Context)
      return Range_Attribute_Reference_Ptr;

   function Range_Attribute
     (Element : Range_Attribute_Reference_Node) return Asis.Expression;

   procedure Set_Range_Attribute
     (Element : in out Range_Attribute_Reference_Node;
      Value   : in     Asis.Expression);

   function Constraint_Kind (Element : Range_Attribute_Reference_Node)
      return Asis.Constraint_Kinds;

   function Children (Element : access Range_Attribute_Reference_Node)
     return Traverse_List;

   function Clone
     (Element : Range_Attribute_Reference_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Range_Attribute_Reference_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------
   -- Simple_Expression_Range_Node --
   ----------------------------------

   type Simple_Expression_Range_Node is 
      new Constraint_Node with private;

   type Simple_Expression_Range_Ptr is
      access all Simple_Expression_Range_Node;
   for Simple_Expression_Range_Ptr'Storage_Pool use Lists.Pool;

   function New_Simple_Expression_Range_Node
     (The_Context : ASIS.Context)
      return Simple_Expression_Range_Ptr;

   function Lower_Bound
     (Element : Simple_Expression_Range_Node) return Asis.Expression;

   procedure Set_Lower_Bound
     (Element : in out Simple_Expression_Range_Node;
      Value   : in     Asis.Expression);

   function Upper_Bound
     (Element : Simple_Expression_Range_Node) return Asis.Expression;

   procedure Set_Upper_Bound
     (Element : in out Simple_Expression_Range_Node;
      Value   : in     Asis.Expression);

   function Constraint_Kind (Element : Simple_Expression_Range_Node)
      return Asis.Constraint_Kinds;

   function Children (Element : access Simple_Expression_Range_Node)
     return Traverse_List;

   function Clone
     (Element : Simple_Expression_Range_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Simple_Expression_Range_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------
   -- Digits_Constraint_Node --
   ----------------------------

   type Digits_Constraint_Node is 
      new Constraint_Node with private;

   type Digits_Constraint_Ptr is
      access all Digits_Constraint_Node;
   for Digits_Constraint_Ptr'Storage_Pool use Lists.Pool;

   function New_Digits_Constraint_Node
     (The_Context : ASIS.Context)
      return Digits_Constraint_Ptr;

   function Digits_Expression
     (Element : Digits_Constraint_Node) return Asis.Expression;

   procedure Set_Digits_Expression
     (Element : in out Digits_Constraint_Node;
      Value   : in     Asis.Expression);

   function Real_Range_Constraint
     (Element : Digits_Constraint_Node) return Asis.Range_Constraint;

   procedure Set_Real_Range_Constraint
     (Element : in out Digits_Constraint_Node;
      Value   : in     Asis.Range_Constraint);

   function Constraint_Kind (Element : Digits_Constraint_Node)
      return Asis.Constraint_Kinds;

   function Children (Element : access Digits_Constraint_Node)
     return Traverse_List;

   function Clone
     (Element : Digits_Constraint_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Digits_Constraint_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------
   -- Delta_Constraint_Node --
   ---------------------------

   type Delta_Constraint_Node is 
      new Constraint_Node with private;

   type Delta_Constraint_Ptr is
      access all Delta_Constraint_Node;
   for Delta_Constraint_Ptr'Storage_Pool use Lists.Pool;

   function New_Delta_Constraint_Node
     (The_Context : ASIS.Context)
      return Delta_Constraint_Ptr;

   function Delta_Expression
     (Element : Delta_Constraint_Node) return Asis.Expression;

   procedure Set_Delta_Expression
     (Element : in out Delta_Constraint_Node;
      Value   : in     Asis.Expression);

   function Real_Range_Constraint
     (Element : Delta_Constraint_Node) return Asis.Range_Constraint;

   procedure Set_Real_Range_Constraint
     (Element : in out Delta_Constraint_Node;
      Value   : in     Asis.Range_Constraint);

   function Constraint_Kind (Element : Delta_Constraint_Node)
      return Asis.Constraint_Kinds;

   function Children (Element : access Delta_Constraint_Node)
     return Traverse_List;

   function Clone
     (Element : Delta_Constraint_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Delta_Constraint_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------
   -- Index_Constraint_Node --
   ---------------------------

   type Index_Constraint_Node is 
      new Constraint_Node with private;

   type Index_Constraint_Ptr is
      access all Index_Constraint_Node;
   for Index_Constraint_Ptr'Storage_Pool use Lists.Pool;

   function New_Index_Constraint_Node
     (The_Context : ASIS.Context)
      return Index_Constraint_Ptr;

   function Discrete_Ranges
     (Element : Index_Constraint_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Discrete_Ranges
     (Element : in out Index_Constraint_Node;
      Value   : in     Asis.Element);

   function Discrete_Ranges_List
     (Element : Index_Constraint_Node) return Asis.Element;

   function Constraint_Kind (Element : Index_Constraint_Node)
      return Asis.Constraint_Kinds;

   function Children (Element : access Index_Constraint_Node)
     return Traverse_List;

   function Clone
     (Element : Index_Constraint_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Index_Constraint_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------
   -- Discriminant_Constraint_Node --
   ----------------------------------

   type Discriminant_Constraint_Node is 
      new Constraint_Node with private;

   type Discriminant_Constraint_Ptr is
      access all Discriminant_Constraint_Node;
   for Discriminant_Constraint_Ptr'Storage_Pool use Lists.Pool;

   function New_Discriminant_Constraint_Node
     (The_Context : ASIS.Context)
      return Discriminant_Constraint_Ptr;

   function Discriminant_Associations
     (Element : Discriminant_Constraint_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Discriminant_Associations
     (Element : in out Discriminant_Constraint_Node;
      Value   : in     Asis.Element);

   function Discriminant_Associations_List
     (Element : Discriminant_Constraint_Node) return Asis.Element;

   function Normalized_Discriminant_Associations
     (Element : Discriminant_Constraint_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Normalized_Discriminant_Associations
     (Element : in out Discriminant_Constraint_Node;
      Item    : in     Asis.Element);

   function Constraint_Kind (Element : Discriminant_Constraint_Node)
      return Asis.Constraint_Kinds;

   function Children (Element : access Discriminant_Constraint_Node)
     return Traverse_List;

   function Clone
     (Element : Discriminant_Constraint_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Discriminant_Constraint_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Range_Attribute_Reference_Node is 
      new Constraint_Node with
      record
         Range_Attribute                : aliased Asis.Expression;
      end record;


   type Simple_Expression_Range_Node is 
      new Constraint_Node with
      record
         Lower_Bound                    : aliased Asis.Expression;
         Upper_Bound                    : aliased Asis.Expression;
      end record;


   type Digits_Constraint_Node is 
      new Constraint_Node with
      record
         Digits_Expression              : aliased Asis.Expression;
         Real_Range_Constraint          : aliased Asis.Range_Constraint;
      end record;


   type Delta_Constraint_Node is 
      new Constraint_Node with
      record
         Delta_Expression               : aliased Asis.Expression;
         Real_Range_Constraint          : aliased Asis.Range_Constraint;
      end record;


   type Index_Constraint_Node is 
      new Constraint_Node with
      record
         Discrete_Ranges                : aliased Primary_Definition_Lists.List;
      end record;


   type Discriminant_Constraint_Node is 
      new Constraint_Node with
      record
         Discriminant_Associations      : aliased Primary_Association_Lists.List;
         Normalized_Discriminant_Associations : aliased Secondary_Association_Lists.List_Node;
      end record;

end Asis.Gela.Elements.Defs.Const;
