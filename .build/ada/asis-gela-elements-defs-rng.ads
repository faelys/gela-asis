
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
  
package Asis.Gela.Elements.Defs.Rng is

   --------------------------------------
   -- Discrete_Subtype_Indication_Node --
   --------------------------------------

   type Discrete_Subtype_Indication_Node is 
      new Discrete_Range_Node with private;

   type Discrete_Subtype_Indication_Ptr is
      access all Discrete_Subtype_Indication_Node;
   for Discrete_Subtype_Indication_Ptr'Storage_Pool use Lists.Pool;

   function New_Discrete_Subtype_Indication_Node
     (The_Context : ASIS.Context)
      return Discrete_Subtype_Indication_Ptr;

   function Get_Subtype_Mark
     (Element : Discrete_Subtype_Indication_Node) return Asis.Expression;

   procedure Set_Subtype_Mark
     (Element : in out Discrete_Subtype_Indication_Node;
      Value   : in     Asis.Expression);

   function Subtype_Constraint
     (Element : Discrete_Subtype_Indication_Node) return Asis.Constraint;

   procedure Set_Subtype_Constraint
     (Element : in out Discrete_Subtype_Indication_Node;
      Value   : in     Asis.Constraint);

   function Discrete_Range_Kind (Element : Discrete_Subtype_Indication_Node)
      return Asis.Discrete_Range_Kinds;

   function Children (Element : access Discrete_Subtype_Indication_Node)
     return Traverse_List;

   function Clone
     (Element : Discrete_Subtype_Indication_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Discrete_Subtype_Indication_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------------------
   -- Discrete_Range_Attribute_Reference_Node --
   ---------------------------------------------

   type Discrete_Range_Attribute_Reference_Node is 
      new Discrete_Range_Node with private;

   type Discrete_Range_Attribute_Reference_Ptr is
      access all Discrete_Range_Attribute_Reference_Node;
   for Discrete_Range_Attribute_Reference_Ptr'Storage_Pool use Lists.Pool;

   function New_Discrete_Range_Attribute_Reference_Node
     (The_Context : ASIS.Context)
      return Discrete_Range_Attribute_Reference_Ptr;

   function Range_Attribute
     (Element : Discrete_Range_Attribute_Reference_Node) return Asis.Expression;

   procedure Set_Range_Attribute
     (Element : in out Discrete_Range_Attribute_Reference_Node;
      Value   : in     Asis.Expression);

   function Discrete_Range_Kind (Element : Discrete_Range_Attribute_Reference_Node)
      return Asis.Discrete_Range_Kinds;

   function Children (Element : access Discrete_Range_Attribute_Reference_Node)
     return Traverse_List;

   function Clone
     (Element : Discrete_Range_Attribute_Reference_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Discrete_Range_Attribute_Reference_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------------------
   -- Discrete_Simple_Expression_Range_Node --
   -------------------------------------------

   type Discrete_Simple_Expression_Range_Node is 
      new Discrete_Range_Node with private;

   type Discrete_Simple_Expression_Range_Ptr is
      access all Discrete_Simple_Expression_Range_Node;
   for Discrete_Simple_Expression_Range_Ptr'Storage_Pool use Lists.Pool;

   function New_Discrete_Simple_Expression_Range_Node
     (The_Context : ASIS.Context)
      return Discrete_Simple_Expression_Range_Ptr;

   function Lower_Bound
     (Element : Discrete_Simple_Expression_Range_Node) return Asis.Expression;

   procedure Set_Lower_Bound
     (Element : in out Discrete_Simple_Expression_Range_Node;
      Value   : in     Asis.Expression);

   function Upper_Bound
     (Element : Discrete_Simple_Expression_Range_Node) return Asis.Expression;

   procedure Set_Upper_Bound
     (Element : in out Discrete_Simple_Expression_Range_Node;
      Value   : in     Asis.Expression);

   function Discrete_Range_Kind (Element : Discrete_Simple_Expression_Range_Node)
      return Asis.Discrete_Range_Kinds;

   function Children (Element : access Discrete_Simple_Expression_Range_Node)
     return Traverse_List;

   function Clone
     (Element : Discrete_Simple_Expression_Range_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Discrete_Simple_Expression_Range_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Discrete_Subtype_Indication_Node is 
      new Discrete_Range_Node with
      record
         Subtype_Mark                   : aliased Asis.Expression;
         Subtype_Constraint             : aliased Asis.Constraint;
      end record;


   type Discrete_Range_Attribute_Reference_Node is 
      new Discrete_Range_Node with
      record
         Range_Attribute                : aliased Asis.Expression;
      end record;


   type Discrete_Simple_Expression_Range_Node is 
      new Discrete_Range_Node with
      record
         Lower_Bound                    : aliased Asis.Expression;
         Upper_Bound                    : aliased Asis.Expression;
      end record;

end Asis.Gela.Elements.Defs.Rng;
