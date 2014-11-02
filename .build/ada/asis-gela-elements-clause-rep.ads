
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
  
package Asis.Gela.Elements.Clause.Rep is

   ---------------------------------------
   -- Record_Representation_Clause_Node --
   ---------------------------------------

   type Record_Representation_Clause_Node is 
      new Representation_Clause_Node with private;

   type Record_Representation_Clause_Ptr is
      access all Record_Representation_Clause_Node;
   for Record_Representation_Clause_Ptr'Storage_Pool use Lists.Pool;

   function New_Record_Representation_Clause_Node
     (The_Context : ASIS.Context)
      return Record_Representation_Clause_Ptr;

   function Mod_Clause_Expression
     (Element : Record_Representation_Clause_Node) return Asis.Expression;

   procedure Set_Mod_Clause_Expression
     (Element : in out Record_Representation_Clause_Node;
      Value   : in     Asis.Expression);

   function Component_Clauses
     (Element : Record_Representation_Clause_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Component_Clauses
     (Element : in out Record_Representation_Clause_Node;
      Value   : in     Asis.Element);

   function Component_Clauses_List
     (Element : Record_Representation_Clause_Node) return Asis.Element;

   function Representation_Clause_Kind (Element : Record_Representation_Clause_Node)
      return Asis.Representation_Clause_Kinds;

   function Children (Element : access Record_Representation_Clause_Node)
     return Traverse_List;

   function Clone
     (Element : Record_Representation_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Record_Representation_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------
   -- At_Clause_Node --
   --------------------

   type At_Clause_Node is 
      new Representation_Clause_Node with private;

   type At_Clause_Ptr is
      access all At_Clause_Node;
   for At_Clause_Ptr'Storage_Pool use Lists.Pool;

   function New_At_Clause_Node
     (The_Context : ASIS.Context)
      return At_Clause_Ptr;

   function Representation_Clause_Expression
     (Element : At_Clause_Node) return Asis.Expression;

   procedure Set_Representation_Clause_Expression
     (Element : in out At_Clause_Node;
      Value   : in     Asis.Expression);

   function Representation_Clause_Kind (Element : At_Clause_Node)
      return Asis.Representation_Clause_Kinds;

   function Children (Element : access At_Clause_Node)
     return Traverse_List;

   function Clone
     (Element : At_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access At_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------------
   -- Attribute_Definition_Clause_Node --
   --------------------------------------

   type Attribute_Definition_Clause_Node is 
      new At_Clause_Node with private;

   type Attribute_Definition_Clause_Ptr is
      access all Attribute_Definition_Clause_Node;
   for Attribute_Definition_Clause_Ptr'Storage_Pool use Lists.Pool;

   function New_Attribute_Definition_Clause_Node
     (The_Context : ASIS.Context)
      return Attribute_Definition_Clause_Ptr;

   function Representation_Clause_Kind (Element : Attribute_Definition_Clause_Node)
      return Asis.Representation_Clause_Kinds;

   function Clone
     (Element : Attribute_Definition_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Attribute_Definition_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------------------
   -- Enumeration_Representation_Clause_Node --
   --------------------------------------------

   type Enumeration_Representation_Clause_Node is 
      new At_Clause_Node with private;

   type Enumeration_Representation_Clause_Ptr is
      access all Enumeration_Representation_Clause_Node;
   for Enumeration_Representation_Clause_Ptr'Storage_Pool use Lists.Pool;

   function New_Enumeration_Representation_Clause_Node
     (The_Context : ASIS.Context)
      return Enumeration_Representation_Clause_Ptr;

   function Representation_Clause_Kind (Element : Enumeration_Representation_Clause_Node)
      return Asis.Representation_Clause_Kinds;

   function Clone
     (Element : Enumeration_Representation_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Enumeration_Representation_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Record_Representation_Clause_Node is 
      new Representation_Clause_Node with
      record
         Mod_Clause_Expression          : aliased Asis.Expression;
         Component_Clauses              : aliased Primary_Clause_Lists.List;
      end record;


   type At_Clause_Node is 
      new Representation_Clause_Node with
      record
         Representation_Clause_Expression : aliased Asis.Expression;
      end record;


   type Attribute_Definition_Clause_Node is 
      new At_Clause_Node with
      record
         null;
      end record;


   type Enumeration_Representation_Clause_Node is 
      new At_Clause_Node with
      record
         null;
      end record;

end Asis.Gela.Elements.Clause.Rep;
