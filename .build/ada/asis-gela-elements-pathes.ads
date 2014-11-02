
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
  
package Asis.Gela.Elements.Pathes is

   ------------------
   -- If_Path_Node --
   ------------------

   type If_Path_Node is 
      new Path_Node with private;

   type If_Path_Ptr is
      access all If_Path_Node;
   for If_Path_Ptr'Storage_Pool use Lists.Pool;

   function New_If_Path_Node
     (The_Context : ASIS.Context)
      return If_Path_Ptr;

   function Condition_Expression
     (Element : If_Path_Node) return Asis.Expression;

   procedure Set_Condition_Expression
     (Element : in out If_Path_Node;
      Value   : in     Asis.Expression);

   function Path_Kind (Element : If_Path_Node)
      return Asis.Path_Kinds;

   function Children (Element : access If_Path_Node)
     return Traverse_List;

   function Clone
     (Element : If_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access If_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------
   -- Elsif_Path_Node --
   ---------------------

   type Elsif_Path_Node is 
      new If_Path_Node with private;

   type Elsif_Path_Ptr is
      access all Elsif_Path_Node;
   for Elsif_Path_Ptr'Storage_Pool use Lists.Pool;

   function New_Elsif_Path_Node
     (The_Context : ASIS.Context)
      return Elsif_Path_Ptr;

   function Path_Kind (Element : Elsif_Path_Node)
      return Asis.Path_Kinds;

   function Clone
     (Element : Elsif_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Elsif_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------
   -- Else_Path_Node --
   --------------------

   type Else_Path_Node is 
      new Path_Node with private;

   type Else_Path_Ptr is
      access all Else_Path_Node;
   for Else_Path_Ptr'Storage_Pool use Lists.Pool;

   function New_Else_Path_Node
     (The_Context : ASIS.Context)
      return Else_Path_Ptr;

   function Path_Kind (Element : Else_Path_Node)
      return Asis.Path_Kinds;

   function Clone
     (Element : Else_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Else_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------
   -- Case_Path_Node --
   --------------------

   type Case_Path_Node is 
      new Path_Node with private;

   type Case_Path_Ptr is
      access all Case_Path_Node;
   for Case_Path_Ptr'Storage_Pool use Lists.Pool;

   function New_Case_Path_Node
     (The_Context : ASIS.Context)
      return Case_Path_Ptr;

   function Case_Statement_Alternative_Choices
     (Element : Case_Path_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Case_Statement_Alternative_Choices
     (Element : in out Case_Path_Node;
      Value   : in     Asis.Element);

   function Case_Statement_Alternative_Choices_List
     (Element : Case_Path_Node) return Asis.Element;

   function Pragmas
     (Element : Case_Path_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Pragmas
     (Element : in out Case_Path_Node;
      Value   : in     Asis.Element);

   function Pragmas_List
     (Element : Case_Path_Node) return Asis.Element;

   function Path_Kind (Element : Case_Path_Node)
      return Asis.Path_Kinds;

   function Children (Element : access Case_Path_Node)
     return Traverse_List;

   function Clone
     (Element : Case_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Case_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------
   -- Select_Path_Node --
   ----------------------

   type Select_Path_Node is 
      new Path_Node with private;

   type Select_Path_Ptr is
      access all Select_Path_Node;
   for Select_Path_Ptr'Storage_Pool use Lists.Pool;

   function New_Select_Path_Node
     (The_Context : ASIS.Context)
      return Select_Path_Ptr;

   function Guard
     (Element : Select_Path_Node) return Asis.Expression;

   procedure Set_Guard
     (Element : in out Select_Path_Node;
      Value   : in     Asis.Expression);

   function Pragmas
     (Element : Select_Path_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Pragmas
     (Element : in out Select_Path_Node;
      Value   : in     Asis.Element);

   function Pragmas_List
     (Element : Select_Path_Node) return Asis.Element;

   function Path_Kind (Element : Select_Path_Node)
      return Asis.Path_Kinds;

   function Children (Element : access Select_Path_Node)
     return Traverse_List;

   function Clone
     (Element : Select_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Select_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------
   -- Or_Path_Node --
   ------------------

   type Or_Path_Node is 
      new Select_Path_Node with private;

   type Or_Path_Ptr is
      access all Or_Path_Node;
   for Or_Path_Ptr'Storage_Pool use Lists.Pool;

   function New_Or_Path_Node
     (The_Context : ASIS.Context)
      return Or_Path_Ptr;

   function Path_Kind (Element : Or_Path_Node)
      return Asis.Path_Kinds;

   function Clone
     (Element : Or_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Or_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------
   -- Then_Abort_Path_Node --
   --------------------------

   type Then_Abort_Path_Node is 
      new Path_Node with private;

   type Then_Abort_Path_Ptr is
      access all Then_Abort_Path_Node;
   for Then_Abort_Path_Ptr'Storage_Pool use Lists.Pool;

   function New_Then_Abort_Path_Node
     (The_Context : ASIS.Context)
      return Then_Abort_Path_Ptr;

   function Path_Kind (Element : Then_Abort_Path_Node)
      return Asis.Path_Kinds;

   function Clone
     (Element : Then_Abort_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Then_Abort_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type If_Path_Node is 
      new Path_Node with
      record
         Condition_Expression           : aliased Asis.Expression;
      end record;


   type Elsif_Path_Node is 
      new If_Path_Node with
      record
         null;
      end record;


   type Else_Path_Node is 
      new Path_Node with
      record
         null;
      end record;


   type Case_Path_Node is 
      new Path_Node with
      record
         Case_Statement_Alternative_Choices : aliased Primary_Choise_Lists.List;
         Pragmas                        : aliased Primary_Pragma_Lists.List;
      end record;


   type Select_Path_Node is 
      new Path_Node with
      record
         Guard                          : aliased Asis.Expression;
         Pragmas                        : aliased Primary_Pragma_Lists.List;
      end record;


   type Or_Path_Node is 
      new Select_Path_Node with
      record
         null;
      end record;


   type Then_Abort_Path_Node is 
      new Path_Node with
      record
         null;
      end record;

end Asis.Gela.Elements.Pathes;
