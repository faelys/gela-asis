
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
  
with Asis.Gela.Lists;                  use Asis.Gela.Lists;
with Asis.Gela.Visibility;

package Asis.Gela.Elements is

   -----------------------
   -- Base_Element_Node --
   -----------------------

   type Base_Element_Node is abstract
      new Element_Node with private;

   type Base_Element_Ptr is
      access all Base_Element_Node;
   for Base_Element_Ptr'Storage_Pool use Lists.Pool;

   function Enclosing_Element
     (Element : Base_Element_Node) return Asis.Element;

   procedure Set_Enclosing_Element
     (Element : in out Base_Element_Node;
      Value   : in     Asis.Element);

   function Next_Element
     (Element : Base_Element_Node) return Asis.Element;

   procedure Set_Next_Element
     (Element : in out Base_Element_Node;
      Value   : in     Asis.Element);

   function Is_Part_Of_Implicit
     (Element : Base_Element_Node) return Boolean;

   procedure Set_Is_Part_Of_Implicit
     (Element : in out Base_Element_Node;
      Value   : in     Boolean);

   function Is_Part_Of_Inherited
     (Element : Base_Element_Node) return Boolean;

   procedure Set_Is_Part_Of_Inherited
     (Element : in out Base_Element_Node;
      Value   : in     Boolean);

   function Is_Part_Of_Instance
     (Element : Base_Element_Node) return Boolean;

   procedure Set_Is_Part_Of_Instance
     (Element : in out Base_Element_Node;
      Value   : in     Boolean);

   function Start_Position
     (Element : Base_Element_Node) return Asis.Text_Position;

   procedure Set_Start_Position
     (Element : in out Base_Element_Node;
      Value   : in     Asis.Text_Position);

   function End_Position
     (Element : Base_Element_Node) return Asis.Text_Position;

   procedure Set_End_Position
     (Element : in out Base_Element_Node;
      Value   : in     Asis.Text_Position);

   function Enclosing_Compilation_Unit
     (Element : Base_Element_Node) return Asis.Compilation_Unit;

   procedure Set_Enclosing_Compilation_Unit
     (Element : in out Base_Element_Node;
      Value   : in     Asis.Compilation_Unit);

   function Hash
     (Element : Base_Element_Node) return Asis.ASIS_Integer;

   procedure Set_Hash
     (Element : in out Base_Element_Node;
      Value   : in     Asis.ASIS_Integer);

   -----------------
   -- Pragma_Node --
   -----------------

   type Pragma_Node is 
      new Base_Element_Node with private;

   type Pragma_Ptr is
      access all Pragma_Node;
   for Pragma_Ptr'Storage_Pool use Lists.Pool;

   function New_Pragma_Node
     (The_Context : ASIS.Context)
      return Pragma_Ptr;

   function Pragma_Kind
     (Element : Pragma_Node) return Asis.Pragma_Kinds;

   procedure Set_Pragma_Kind
     (Element : in out Pragma_Node;
      Value   : in     Asis.Pragma_Kinds);

   function Pragma_Name_Image
     (Element : Pragma_Node) return Wide_String;

   procedure Set_Pragma_Name_Image
     (Element : in out Pragma_Node;
      Value   : in     Wide_String);

   function Pragma_Argument_Associations
     (Element : Pragma_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Pragma_Argument_Associations
     (Element : in out Pragma_Node;
      Value   : in     Asis.Element);

   function Pragma_Argument_Associations_List
     (Element : Pragma_Node) return Asis.Element;

   function Element_Kind (Element : Pragma_Node)
      return Asis.Element_Kinds;

   function Children (Element : access Pragma_Node)
     return Traverse_List;

   function Clone
     (Element : Pragma_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Pragma_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------
   -- Defining_Name_Node --
   ------------------------

   type Defining_Name_Node is abstract
      new Base_Element_Node with private;

   type Defining_Name_Ptr is
      access all Defining_Name_Node;
   for Defining_Name_Ptr'Storage_Pool use Lists.Pool;

   function Defining_Name_Image
     (Element : Defining_Name_Node) return Wide_String;

   procedure Set_Defining_Name_Image
     (Element : in out Defining_Name_Node;
      Value   : in     Wide_String);

   function Corresponding_Constant_Declaration
     (Element : Defining_Name_Node) return Asis.Element;

   procedure Set_Corresponding_Constant_Declaration
     (Element : in out Defining_Name_Node;
      Value   : in     Asis.Element);

   function References
     (Element : Defining_Name_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_References
     (Element : in out Defining_Name_Node;
      Item    : in     Asis.Element);

   function Corresponding_Generic_Element
     (Element : Defining_Name_Node) return Asis.Defining_Name;

   procedure Set_Corresponding_Generic_Element
     (Element : in out Defining_Name_Node;
      Value   : in     Asis.Defining_Name);

   function Override
     (Element : Defining_Name_Node) return Asis.Defining_Name;

   procedure Set_Override
     (Element : in out Defining_Name_Node;
      Value   : in     Asis.Defining_Name);

   function Place
     (Element : Defining_Name_Node) return Visibility.Region_Item_Access;

   procedure Set_Place
     (Element : in out Defining_Name_Node;
      Value   : in     Visibility.Region_Item_Access);

   function Element_Kind (Element : Defining_Name_Node)
      return Asis.Element_Kinds;

   ----------------------
   -- Declaration_Node --
   ----------------------

   type Declaration_Node is abstract
      new Base_Element_Node with private;

   type Declaration_Ptr is
      access all Declaration_Node;
   for Declaration_Ptr'Storage_Pool use Lists.Pool;

   function Declaration_Origin
     (Element : Declaration_Node) return Asis.Declaration_Origins;

   procedure Set_Declaration_Origin
     (Element : in out Declaration_Node;
      Value   : in     Asis.Declaration_Origins);

   function Name
     (Element : Declaration_Node) return Asis.Element;

   procedure Set_Name
     (Element : in out Declaration_Node;
      Value   : in     Asis.Element);

   function Names
     (Element : Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Names
     (Element : in out Declaration_Node;
      Value   : in     Asis.Element);

   function Names_List
     (Element : Declaration_Node) return Asis.Element;

   function Corresponding_Representation_Clauses
     (Element : Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Corresponding_Representation_Clauses
     (Element : in out Declaration_Node;
      Item    : in     Asis.Element);

   function Corresponding_Pragmas
     (Element : Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Corresponding_Pragmas
     (Element : in out Declaration_Node;
      Item    : in     Asis.Element);

   function Place
     (Element : Declaration_Node) return Visibility.Region_Item_Access;

   procedure Set_Place
     (Element : in out Declaration_Node;
      Value   : in     Visibility.Region_Item_Access);

   function Element_Kind (Element : Declaration_Node)
      return Asis.Element_Kinds;

   function Children (Element : access Declaration_Node)
     return Traverse_List;

   ---------------------
   -- Definition_Node --
   ---------------------

   type Definition_Node is abstract
      new Base_Element_Node with private;

   type Definition_Ptr is
      access all Definition_Node;
   for Definition_Ptr'Storage_Pool use Lists.Pool;

   function Element_Kind (Element : Definition_Node)
      return Asis.Element_Kinds;

   ---------------------
   -- Expression_Node --
   ---------------------

   type Expression_Node is abstract
      new Base_Element_Node with private;

   type Expression_Ptr is
      access all Expression_Node;
   for Expression_Ptr'Storage_Pool use Lists.Pool;

   function Corresponding_Expression_Type
     (Element : Expression_Node) return Asis.Element;

   procedure Set_Corresponding_Expression_Type
     (Element : in out Expression_Node;
      Value   : in     Asis.Element);

   function Is_Static_Expression
     (Element : Expression_Node) return Asis.Gela.Fuzzy_Boolean;

   procedure Set_Is_Static_Expression
     (Element : in out Expression_Node;
      Value   : in     Asis.Gela.Fuzzy_Boolean);

   function Element_Kind (Element : Expression_Node)
      return Asis.Element_Kinds;

   ----------------------
   -- Association_Node --
   ----------------------

   type Association_Node is abstract
      new Base_Element_Node with private;

   type Association_Ptr is
      access all Association_Node;
   for Association_Ptr'Storage_Pool use Lists.Pool;

   function Element_Kind (Element : Association_Node)
      return Asis.Element_Kinds;

   --------------------
   -- Statement_Node --
   --------------------

   type Statement_Node is abstract
      new Base_Element_Node with private;

   type Statement_Ptr is
      access all Statement_Node;
   for Statement_Ptr'Storage_Pool use Lists.Pool;

   function Label_Names
     (Element : Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Label_Names
     (Element : in out Statement_Node;
      Value   : in     Asis.Element);

   function Label_Names_List
     (Element : Statement_Node) return Asis.Element;

   function Corresponding_Pragmas
     (Element : Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Corresponding_Pragmas
     (Element : in out Statement_Node;
      Item    : in     Asis.Element);

   function Place
     (Element : Statement_Node) return Visibility.Region_Item_Access;

   procedure Set_Place
     (Element : in out Statement_Node;
      Value   : in     Visibility.Region_Item_Access);

   function Element_Kind (Element : Statement_Node)
      return Asis.Element_Kinds;

   function Children (Element : access Statement_Node)
     return Traverse_List;

   ---------------
   -- Path_Node --
   ---------------

   type Path_Node is abstract
      new Base_Element_Node with private;

   type Path_Ptr is
      access all Path_Node;
   for Path_Ptr'Storage_Pool use Lists.Pool;

   function Sequence_Of_Statements
     (Element : Path_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Sequence_Of_Statements
     (Element : in out Path_Node;
      Value   : in     Asis.Element);

   function Sequence_Of_Statements_List
     (Element : Path_Node) return Asis.Element;

   function Element_Kind (Element : Path_Node)
      return Asis.Element_Kinds;

   function Children (Element : access Path_Node)
     return Traverse_List;

   -----------------
   -- Clause_Node --
   -----------------

   type Clause_Node is abstract
      new Base_Element_Node with private;

   type Clause_Ptr is
      access all Clause_Node;
   for Clause_Ptr'Storage_Pool use Lists.Pool;

   function Place
     (Element : Clause_Node) return Visibility.Region_Item_Access;

   procedure Set_Place
     (Element : in out Clause_Node;
      Value   : in     Visibility.Region_Item_Access);

   function Element_Kind (Element : Clause_Node)
      return Asis.Element_Kinds;

   ----------------------------
   -- Exception_Handler_Node --
   ----------------------------

   type Exception_Handler_Node is 
      new Base_Element_Node with private;

   type Exception_Handler_Ptr is
      access all Exception_Handler_Node;
   for Exception_Handler_Ptr'Storage_Pool use Lists.Pool;

   function New_Exception_Handler_Node
     (The_Context : ASIS.Context)
      return Exception_Handler_Ptr;

   function Choice_Parameter_Specification
     (Element : Exception_Handler_Node) return Asis.Element;

   procedure Set_Choice_Parameter_Specification
     (Element : in out Exception_Handler_Node;
      Value   : in     Asis.Element);

   function Exception_Choices
     (Element : Exception_Handler_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Exception_Choices
     (Element : in out Exception_Handler_Node;
      Value   : in     Asis.Element);

   function Exception_Choices_List
     (Element : Exception_Handler_Node) return Asis.Element;

   function Handler_Statements
     (Element : Exception_Handler_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Handler_Statements
     (Element : in out Exception_Handler_Node;
      Value   : in     Asis.Element);

   function Handler_Statements_List
     (Element : Exception_Handler_Node) return Asis.Element;

   function Pragmas
     (Element : Exception_Handler_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Pragmas
     (Element : in out Exception_Handler_Node;
      Value   : in     Asis.Element);

   function Pragmas_List
     (Element : Exception_Handler_Node) return Asis.Element;

   function Place
     (Element : Exception_Handler_Node) return Visibility.Region_Item_Access;

   procedure Set_Place
     (Element : in out Exception_Handler_Node;
      Value   : in     Visibility.Region_Item_Access);

   function Element_Kind (Element : Exception_Handler_Node)
      return Asis.Element_Kinds;

   function Children (Element : access Exception_Handler_Node)
     return Traverse_List;

   function Clone
     (Element : Exception_Handler_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Exception_Handler_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Base_Element_Node is abstract
      new Element_Node with
      record
         Enclosing_Element              : aliased Asis.Element;
         Next_Element                   : aliased Asis.Element;
         Is_Part_Of_Implicit            : aliased Boolean := False;
         Is_Part_Of_Inherited           : aliased Boolean := False;
         Is_Part_Of_Instance            : aliased Boolean := False;
         Start_Position                 : aliased Asis.Text_Position;
         End_Position                   : aliased Asis.Text_Position;
         Enclosing_Compilation_Unit     : aliased Asis.Compilation_Unit;
         Hash                           : aliased Asis.ASIS_Integer
           := Next_Hash;
      end record;


   type Pragma_Node is 
      new Base_Element_Node with
      record
         Pragma_Kind                    : aliased Asis.Pragma_Kinds := Not_A_Pragma;
         Pragma_Name_Image              : aliased Unbounded_Wide_String;
         Pragma_Argument_Associations   : aliased Primary_Association_Lists.List;
      end record;


   type Defining_Name_Node is abstract
      new Base_Element_Node with
      record
         Defining_Name_Image            : aliased Unbounded_Wide_String;
         Corresponding_Constant_Declaration : aliased Asis.Element;
         References                     : aliased Secondary_Reference_Lists.List_Node;
         Corresponding_Generic_Element  : aliased Asis.Defining_Name;
         Override                       : aliased Asis.Defining_Name;
         Place                          : aliased Visibility.Region_Item_Access;
      end record;


   type Declaration_Node is abstract
      new Base_Element_Node with
      record
         Declaration_Origin             : aliased Asis.Declaration_Origins
           := An_Explicit_Declaration;
         Name                           : aliased Asis.Element;
         Names                          : aliased Primary_Defining_Name_Lists.List;
         Corresponding_Representation_Clauses : aliased Secondary_Clause_Lists.List_Node;
         Corresponding_Pragmas          : aliased Secondary_Pragma_Lists.List_Node;
         Place                          : aliased Visibility.Region_Item_Access;
      end record;


   type Definition_Node is abstract
      new Base_Element_Node with
      record
         null;
      end record;


   type Expression_Node is abstract
      new Base_Element_Node with
      record
         Corresponding_Expression_Type  : aliased Asis.Element;
         Is_Static_Expression           : aliased Asis.Gela.Fuzzy_Boolean
           := Unknown;
      end record;


   type Association_Node is abstract
      new Base_Element_Node with
      record
         null;
      end record;


   type Statement_Node is abstract
      new Base_Element_Node with
      record
         Label_Names                    : aliased Primary_Defining_Name_Lists.List;
         Corresponding_Pragmas          : aliased Secondary_Pragma_Lists.List_Node;
         Place                          : aliased Visibility.Region_Item_Access;
      end record;


   type Path_Node is abstract
      new Base_Element_Node with
      record
         Sequence_Of_Statements         : aliased Primary_Statement_Lists.List;
      end record;


   type Clause_Node is abstract
      new Base_Element_Node with
      record
         Place                          : aliased Visibility.Region_Item_Access;
      end record;


   type Exception_Handler_Node is 
      new Base_Element_Node with
      record
         Choice_Parameter_Specification : aliased Asis.Element;
         Exception_Choices              : aliased Primary_Choise_Lists.List;
         Handler_Statements             : aliased Primary_Statement_Lists.List;
         Pragmas                        : aliased Primary_Pragma_Lists.List;
         Place                          : aliased Visibility.Region_Item_Access;
      end record;

end Asis.Gela.Elements;
