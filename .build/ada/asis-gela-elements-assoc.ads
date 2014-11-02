
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
  
package Asis.Gela.Elements.Assoc is

   --------------------------------------
   -- Pragma_Argument_Association_Node --
   --------------------------------------

   type Pragma_Argument_Association_Node is 
      new Association_Node with private;

   type Pragma_Argument_Association_Ptr is
      access all Pragma_Argument_Association_Node;
   for Pragma_Argument_Association_Ptr'Storage_Pool use Lists.Pool;

   function New_Pragma_Argument_Association_Node
     (The_Context : ASIS.Context)
      return Pragma_Argument_Association_Ptr;

   function Formal_Parameter
     (Element : Pragma_Argument_Association_Node) return Asis.Identifier;

   procedure Set_Formal_Parameter
     (Element : in out Pragma_Argument_Association_Node;
      Value   : in     Asis.Identifier);

   function Actual_Parameter
     (Element : Pragma_Argument_Association_Node) return Asis.Expression;

   procedure Set_Actual_Parameter
     (Element : in out Pragma_Argument_Association_Node;
      Value   : in     Asis.Expression);

   function Association_Kind (Element : Pragma_Argument_Association_Node)
      return Asis.Association_Kinds;

   function Children (Element : access Pragma_Argument_Association_Node)
     return Traverse_List;

   function Clone
     (Element : Pragma_Argument_Association_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Pragma_Argument_Association_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Parameter_Association_Node --
   --------------------------------

   type Parameter_Association_Node is 
      new Pragma_Argument_Association_Node with private;

   type Parameter_Association_Ptr is
      access all Parameter_Association_Node;
   for Parameter_Association_Ptr'Storage_Pool use Lists.Pool;

   function New_Parameter_Association_Node
     (The_Context : ASIS.Context)
      return Parameter_Association_Ptr;

   function Is_Normalized
     (Element : Parameter_Association_Node) return Boolean;

   procedure Set_Is_Normalized
     (Element : in out Parameter_Association_Node;
      Value   : in     Boolean);

   function Is_Defaulted_Association
     (Element : Parameter_Association_Node) return Boolean;

   procedure Set_Is_Defaulted_Association
     (Element : in out Parameter_Association_Node;
      Value   : in     Boolean);

   function Association_Kind (Element : Parameter_Association_Node)
      return Asis.Association_Kinds;

   function Clone
     (Element : Parameter_Association_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Parameter_Association_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------
   -- Generic_Association_Node --
   ------------------------------

   type Generic_Association_Node is 
      new Parameter_Association_Node with private;

   type Generic_Association_Ptr is
      access all Generic_Association_Node;
   for Generic_Association_Ptr'Storage_Pool use Lists.Pool;

   function New_Generic_Association_Node
     (The_Context : ASIS.Context)
      return Generic_Association_Ptr;

   function Association_Kind (Element : Generic_Association_Node)
      return Asis.Association_Kinds;

   function Clone
     (Element : Generic_Association_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Association_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------------
   -- Discriminant_Association_Node --
   -----------------------------------

   type Discriminant_Association_Node is 
      new Association_Node with private;

   type Discriminant_Association_Ptr is
      access all Discriminant_Association_Node;
   for Discriminant_Association_Ptr'Storage_Pool use Lists.Pool;

   function New_Discriminant_Association_Node
     (The_Context : ASIS.Context)
      return Discriminant_Association_Ptr;

   function Is_Normalized
     (Element : Discriminant_Association_Node) return Boolean;

   procedure Set_Is_Normalized
     (Element : in out Discriminant_Association_Node;
      Value   : in     Boolean);

   function Discriminant_Expression
     (Element : Discriminant_Association_Node) return Asis.Expression;

   procedure Set_Discriminant_Expression
     (Element : in out Discriminant_Association_Node;
      Value   : in     Asis.Expression);

   function Discriminant_Selector_Name
     (Element : Discriminant_Association_Node) return Asis.Expression;

   procedure Set_Discriminant_Selector_Name
     (Element : in out Discriminant_Association_Node;
      Value   : in     Asis.Expression);

   function Discriminant_Selector_Names
     (Element : Discriminant_Association_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Discriminant_Selector_Names
     (Element : in out Discriminant_Association_Node;
      Value   : in     Asis.Element);

   function Discriminant_Selector_Names_List
     (Element : Discriminant_Association_Node) return Asis.Element;

   function Association_Kind (Element : Discriminant_Association_Node)
      return Asis.Association_Kinds;

   function Children (Element : access Discriminant_Association_Node)
     return Traverse_List;

   function Clone
     (Element : Discriminant_Association_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Discriminant_Association_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------------
   -- Record_Component_Association_Node --
   ---------------------------------------

   type Record_Component_Association_Node is 
      new Association_Node with private;

   type Record_Component_Association_Ptr is
      access all Record_Component_Association_Node;
   for Record_Component_Association_Ptr'Storage_Pool use Lists.Pool;

   function New_Record_Component_Association_Node
     (The_Context : ASIS.Context)
      return Record_Component_Association_Ptr;

   function Is_Normalized
     (Element : Record_Component_Association_Node) return Boolean;

   procedure Set_Is_Normalized
     (Element : in out Record_Component_Association_Node;
      Value   : in     Boolean);

   function Component_Expression
     (Element : Record_Component_Association_Node) return Asis.Expression;

   procedure Set_Component_Expression
     (Element : in out Record_Component_Association_Node;
      Value   : in     Asis.Expression);

   function Record_Component_Choice
     (Element : Record_Component_Association_Node) return Asis.Defining_Name;

   procedure Set_Record_Component_Choice
     (Element : in out Record_Component_Association_Node;
      Value   : in     Asis.Defining_Name);

   function Record_Component_Choices
     (Element : Record_Component_Association_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Record_Component_Choices
     (Element : in out Record_Component_Association_Node;
      Value   : in     Asis.Element);

   function Record_Component_Choices_List
     (Element : Record_Component_Association_Node) return Asis.Element;

   function Association_Kind (Element : Record_Component_Association_Node)
      return Asis.Association_Kinds;

   function Children (Element : access Record_Component_Association_Node)
     return Traverse_List;

   function Clone
     (Element : Record_Component_Association_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Record_Component_Association_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------------
   -- Array_Component_Association_Node --
   --------------------------------------

   type Array_Component_Association_Node is 
      new Association_Node with private;

   type Array_Component_Association_Ptr is
      access all Array_Component_Association_Node;
   for Array_Component_Association_Ptr'Storage_Pool use Lists.Pool;

   function New_Array_Component_Association_Node
     (The_Context : ASIS.Context)
      return Array_Component_Association_Ptr;

   function Array_Component_Choices
     (Element : Array_Component_Association_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Array_Component_Choices
     (Element : in out Array_Component_Association_Node;
      Value   : in     Asis.Element);

   function Array_Component_Choices_List
     (Element : Array_Component_Association_Node) return Asis.Element;

   function Component_Expression
     (Element : Array_Component_Association_Node) return Asis.Expression;

   procedure Set_Component_Expression
     (Element : in out Array_Component_Association_Node;
      Value   : in     Asis.Expression);

   function Association_Kind (Element : Array_Component_Association_Node)
      return Asis.Association_Kinds;

   function Children (Element : access Array_Component_Association_Node)
     return Traverse_List;

   function Clone
     (Element : Array_Component_Association_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Array_Component_Association_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Pragma_Argument_Association_Node is 
      new Association_Node with
      record
         Formal_Parameter               : aliased Asis.Identifier;
         Actual_Parameter               : aliased Asis.Expression;
      end record;


   type Parameter_Association_Node is 
      new Pragma_Argument_Association_Node with
      record
         Is_Normalized                  : aliased Boolean := False;
         Is_Defaulted_Association       : aliased Boolean := False;
      end record;


   type Generic_Association_Node is 
      new Parameter_Association_Node with
      record
         null;
      end record;


   type Discriminant_Association_Node is 
      new Association_Node with
      record
         Is_Normalized                  : aliased Boolean := False;
         Discriminant_Expression        : aliased Asis.Expression;
         Discriminant_Selector_Name     : aliased Asis.Expression;
         Discriminant_Selector_Names    : aliased Primary_Choise_Lists.List;
      end record;


   type Record_Component_Association_Node is 
      new Association_Node with
      record
         Is_Normalized                  : aliased Boolean := False;
         Component_Expression           : aliased Asis.Expression;
         Record_Component_Choice        : aliased Asis.Defining_Name;
         Record_Component_Choices       : aliased Primary_Choise_Lists.List;
      end record;


   type Array_Component_Association_Node is 
      new Association_Node with
      record
         Array_Component_Choices        : aliased Primary_Choise_Lists.List;
         Component_Expression           : aliased Asis.Expression;
      end record;

end Asis.Gela.Elements.Assoc;
