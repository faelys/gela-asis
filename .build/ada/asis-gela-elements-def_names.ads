
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
  
package Asis.Gela.Elements.Def_Names is

   ------------------------------
   -- Defining_Identifier_Node --
   ------------------------------

   type Defining_Identifier_Node is 
      new Defining_Name_Node with private;

   type Defining_Identifier_Ptr is
      access all Defining_Identifier_Node;
   for Defining_Identifier_Ptr'Storage_Pool use Lists.Pool;

   function New_Defining_Identifier_Node
     (The_Context : ASIS.Context)
      return Defining_Identifier_Ptr;

   function Defining_Name_Kind (Element : Defining_Identifier_Node)
      return Asis.Defining_Name_Kinds;

   function Clone
     (Element : Defining_Identifier_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ---------------------------------------
   -- Defining_Enumeration_Literal_Node --
   ---------------------------------------

   type Defining_Enumeration_Literal_Node is 
      new Defining_Name_Node with private;

   type Defining_Enumeration_Literal_Ptr is
      access all Defining_Enumeration_Literal_Node;
   for Defining_Enumeration_Literal_Ptr'Storage_Pool use Lists.Pool;

   function New_Defining_Enumeration_Literal_Node
     (The_Context : ASIS.Context)
      return Defining_Enumeration_Literal_Ptr;

   function Position_Number_Image
     (Element : Defining_Enumeration_Literal_Node) return Wide_String;

   procedure Set_Position_Number_Image
     (Element : in out Defining_Enumeration_Literal_Node;
      Value   : in     Wide_String);

   function Representation_Value_Image
     (Element : Defining_Enumeration_Literal_Node) return Wide_String;

   procedure Set_Representation_Value_Image
     (Element : in out Defining_Enumeration_Literal_Node;
      Value   : in     Wide_String);

   function Defining_Name_Kind (Element : Defining_Enumeration_Literal_Node)
      return Asis.Defining_Name_Kinds;

   function Clone
     (Element : Defining_Enumeration_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -------------------------------------
   -- Defining_Character_Literal_Node --
   -------------------------------------

   type Defining_Character_Literal_Node is 
      new Defining_Enumeration_Literal_Node with private;

   type Defining_Character_Literal_Ptr is
      access all Defining_Character_Literal_Node;
   for Defining_Character_Literal_Ptr'Storage_Pool use Lists.Pool;

   function New_Defining_Character_Literal_Node
     (The_Context : ASIS.Context)
      return Defining_Character_Literal_Ptr;

   function Defining_Name_Kind (Element : Defining_Character_Literal_Node)
      return Asis.Defining_Name_Kinds;

   function Clone
     (Element : Defining_Character_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -----------------------------------
   -- Defining_Operator_Symbol_Node --
   -----------------------------------

   type Defining_Operator_Symbol_Node is 
      new Defining_Name_Node with private;

   type Defining_Operator_Symbol_Ptr is
      access all Defining_Operator_Symbol_Node;
   for Defining_Operator_Symbol_Ptr'Storage_Pool use Lists.Pool;

   function New_Defining_Operator_Symbol_Node
     (The_Context : ASIS.Context)
      return Defining_Operator_Symbol_Ptr;

   function Operator_Kind
     (Element : Defining_Operator_Symbol_Node) return Asis.Operator_Kinds;

   procedure Set_Operator_Kind
     (Element : in out Defining_Operator_Symbol_Node;
      Value   : in     Asis.Operator_Kinds);

   function Defining_Name_Kind (Element : Defining_Operator_Symbol_Node)
      return Asis.Defining_Name_Kinds;

   function Clone
     (Element : Defining_Operator_Symbol_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ---------------------------------
   -- Defining_Expanded_Name_Node --
   ---------------------------------

   type Defining_Expanded_Name_Node is 
      new Defining_Name_Node with private;

   type Defining_Expanded_Name_Ptr is
      access all Defining_Expanded_Name_Node;
   for Defining_Expanded_Name_Ptr'Storage_Pool use Lists.Pool;

   function New_Defining_Expanded_Name_Node
     (The_Context : ASIS.Context)
      return Defining_Expanded_Name_Ptr;

   function Defining_Prefix
     (Element : Defining_Expanded_Name_Node) return Asis.Name;

   procedure Set_Defining_Prefix
     (Element : in out Defining_Expanded_Name_Node;
      Value   : in     Asis.Name);

   function Defining_Selector
     (Element : Defining_Expanded_Name_Node) return Asis.Defining_Name;

   procedure Set_Defining_Selector
     (Element : in out Defining_Expanded_Name_Node;
      Value   : in     Asis.Defining_Name);

   function Defining_Name_Kind (Element : Defining_Expanded_Name_Node)
      return Asis.Defining_Name_Kinds;

   function Children (Element : access Defining_Expanded_Name_Node)
     return Traverse_List;

   function Clone
     (Element : Defining_Expanded_Name_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Defining_Expanded_Name_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Defining_Identifier_Node is 
      new Defining_Name_Node with
      record
         null;
      end record;


   type Defining_Enumeration_Literal_Node is 
      new Defining_Name_Node with
      record
         Position_Number_Image          : aliased Unbounded_Wide_String;
         Representation_Value_Image     : aliased Unbounded_Wide_String;
      end record;


   type Defining_Character_Literal_Node is 
      new Defining_Enumeration_Literal_Node with
      record
         null;
      end record;


   type Defining_Operator_Symbol_Node is 
      new Defining_Name_Node with
      record
         Operator_Kind                  : aliased Asis.Operator_Kinds := Not_An_Operator;
      end record;


   type Defining_Expanded_Name_Node is 
      new Defining_Name_Node with
      record
         Defining_Prefix                : aliased Asis.Name;
         Defining_Selector              : aliased Asis.Defining_Name;
      end record;

end Asis.Gela.Elements.Def_Names;
