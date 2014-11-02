
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
  
package Asis.Gela.Elements.Clause is

   -----------------------
   -- Named_Clause_Node --
   -----------------------

   type Named_Clause_Node is abstract
      new Clause_Node with private;

   type Named_Clause_Ptr is
      access all Named_Clause_Node;
   for Named_Clause_Ptr'Storage_Pool use Lists.Pool;

   function Clause_Names
     (Element : Named_Clause_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Clause_Names
     (Element : in out Named_Clause_Node;
      Value   : in     Asis.Element);

   function Clause_Names_List
     (Element : Named_Clause_Node) return Asis.Element;

   function Children (Element : access Named_Clause_Node)
     return Traverse_List;

   -----------------------------
   -- Use_Package_Clause_Node --
   -----------------------------

   type Use_Package_Clause_Node is 
      new Named_Clause_Node with private;

   type Use_Package_Clause_Ptr is
      access all Use_Package_Clause_Node;
   for Use_Package_Clause_Ptr'Storage_Pool use Lists.Pool;

   function New_Use_Package_Clause_Node
     (The_Context : ASIS.Context)
      return Use_Package_Clause_Ptr;

   function Clause_Kind (Element : Use_Package_Clause_Node)
      return Asis.Clause_Kinds;

   function Clone
     (Element : Use_Package_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Use_Package_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------
   -- Use_Type_Clause_Node --
   --------------------------

   type Use_Type_Clause_Node is 
      new Named_Clause_Node with private;

   type Use_Type_Clause_Ptr is
      access all Use_Type_Clause_Node;
   for Use_Type_Clause_Ptr'Storage_Pool use Lists.Pool;

   function New_Use_Type_Clause_Node
     (The_Context : ASIS.Context)
      return Use_Type_Clause_Ptr;

   function Clause_Kind (Element : Use_Type_Clause_Node)
      return Asis.Clause_Kinds;

   function Clone
     (Element : Use_Type_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Use_Type_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------
   -- With_Clause_Node --
   ----------------------

   type With_Clause_Node is 
      new Named_Clause_Node with private;

   type With_Clause_Ptr is
      access all With_Clause_Node;
   for With_Clause_Ptr'Storage_Pool use Lists.Pool;

   function New_With_Clause_Node
     (The_Context : ASIS.Context)
      return With_Clause_Ptr;

   function Has_Limited
     (Element : With_Clause_Node) return Boolean;

   procedure Set_Has_Limited
     (Element : in out With_Clause_Node;
      Value   : in     Boolean);

   function Has_Private
     (Element : With_Clause_Node) return Boolean;

   procedure Set_Has_Private
     (Element : in out With_Clause_Node;
      Value   : in     Boolean);

   function Clause_Kind (Element : With_Clause_Node)
      return Asis.Clause_Kinds;

   function Clone
     (Element : With_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access With_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Representation_Clause_Node --
   --------------------------------

   type Representation_Clause_Node is abstract
      new Clause_Node with private;

   type Representation_Clause_Ptr is
      access all Representation_Clause_Node;
   for Representation_Clause_Ptr'Storage_Pool use Lists.Pool;

   function Representation_Clause_Name
     (Element : Representation_Clause_Node) return Asis.Name;

   procedure Set_Representation_Clause_Name
     (Element : in out Representation_Clause_Node;
      Value   : in     Asis.Name);

   function Clause_Kind (Element : Representation_Clause_Node)
      return Asis.Clause_Kinds;

   function Children (Element : access Representation_Clause_Node)
     return Traverse_List;

   ---------------------------
   -- Component_Clause_Node --
   ---------------------------

   type Component_Clause_Node is 
      new Clause_Node with private;

   type Component_Clause_Ptr is
      access all Component_Clause_Node;
   for Component_Clause_Ptr'Storage_Pool use Lists.Pool;

   function New_Component_Clause_Node
     (The_Context : ASIS.Context)
      return Component_Clause_Ptr;

   function Component_Clause_Position
     (Element : Component_Clause_Node) return Asis.Expression;

   procedure Set_Component_Clause_Position
     (Element : in out Component_Clause_Node;
      Value   : in     Asis.Expression);

   function Component_Clause_Range
     (Element : Component_Clause_Node) return Asis.Discrete_Range;

   procedure Set_Component_Clause_Range
     (Element : in out Component_Clause_Node;
      Value   : in     Asis.Discrete_Range);

   function Representation_Clause_Name
     (Element : Component_Clause_Node) return Asis.Name;

   procedure Set_Representation_Clause_Name
     (Element : in out Component_Clause_Node;
      Value   : in     Asis.Name);

   function Clause_Kind (Element : Component_Clause_Node)
      return Asis.Clause_Kinds;

   function Children (Element : access Component_Clause_Node)
     return Traverse_List;

   function Clone
     (Element : Component_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Component_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Named_Clause_Node is abstract
      new Clause_Node with
      record
         Clause_Names                   : aliased Primary_Expression_Lists.List;
      end record;


   type Use_Package_Clause_Node is 
      new Named_Clause_Node with
      record
         null;
      end record;


   type Use_Type_Clause_Node is 
      new Named_Clause_Node with
      record
         null;
      end record;


   type With_Clause_Node is 
      new Named_Clause_Node with
      record
         Has_Limited                    : aliased Boolean := False;
         Has_Private                    : aliased Boolean := False;
      end record;


   type Representation_Clause_Node is abstract
      new Clause_Node with
      record
         Representation_Clause_Name     : aliased Asis.Name;
      end record;


   type Component_Clause_Node is 
      new Clause_Node with
      record
         Component_Clause_Position      : aliased Asis.Expression;
         Component_Clause_Range         : aliased Asis.Discrete_Range;
         Representation_Clause_Name     : aliased Asis.Name;
      end record;

end Asis.Gela.Elements.Clause;
