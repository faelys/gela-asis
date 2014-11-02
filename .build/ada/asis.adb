------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $
--  Purpose:


with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;

package body Asis is

   procedure Raise_Inappropriate_Element
     (The_Context : Context;
      Raiser      : Wide_String);

   function Get_Context (Item : in Element_Node'Class) return Context;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Text_Position) return Boolean is
   begin
      return Left.Line < Right.Line or else
        (Left.Line = Right.Line and then Left.Column < Right.Column);
   end "<";

   --------------
   -- Assigned --
   --------------

   function Assigned (Item : in Element) return Boolean is
      type Element_Node_Ptr is access all Element_Node'Class;
   begin
      return Element_Node_Ptr (Item) /= null;
   end Assigned;

   --------------
   -- Assigned --
   --------------

   function Assigned (Item : in Context) return Boolean is
      type Context_Node_Ptr is access all Context_Node'Class;
   begin
      return Context_Node_Ptr (Item) /= null;
   end Assigned;

   --------------
   -- Assigned --
   --------------

   function Assigned (Item : in Compilation_Unit) return Boolean is
      type Comp_Unit_Node_Ptr is access all Compilation_Unit_Node'Class;
   begin
      return Comp_Unit_Node_Ptr (Item) /= null;
   end Assigned;

   -------------------
   -- Check_Context --
   -------------------

   procedure Check_Context (The_Context : Asis.Context) is
   begin
      if not Assigned (The_Context)
        or else not Is_Open (The_Context.all)
      then
         Implementation.Set_Status
           (Errors.Value_Error, "Null or unopen context");
         raise Exceptions.ASIS_Inappropriate_Context;
      end if;
   end Check_Context;

   -----------------------
   -- Check_Nil_Element --
   -----------------------

   procedure Check_Nil_Element
     (Element : Asis.Element;
      Raiser  : Wide_String := "")
   is
   begin
      if not Assigned (Element) then
         Raise_Inappropriate_Element (Raiser);
      end if;
   end Check_Nil_Element;

   --------------------
   -- Check_Nil_Unit --
   --------------------

   procedure Check_Nil_Unit
     (Unit    : Asis.Compilation_Unit;
      Raiser  : Wide_String := "")
   is
   begin
      if not Assigned (Unit) then
         Implementation.Set_Status
           (Errors.Value_Error, "Null compilation unit " & Raiser);
         raise Exceptions.ASIS_Inappropriate_Compilation_Unit;
      end if;
   end Check_Nil_Unit;

   --------------
   -- Children --
   --------------

   function Children (Item : access Element_Node) return Traverse_List is
   begin
      return (1 .. 0 => (False, null));
   end Children;

   -----------
   -- Clone --
   -----------

   function Clone
     (Object : Cloner;
      Item   : Element;
      Parent : Element) return Element
   is
   begin
      return Copy (Source => Item, Cloner => Object, Parent => Parent);
   end Clone;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Source : in     Element;
      Target : access Element_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Element) is
   begin
      null;
   end Copy;

   ----------
   -- Copy --
   ----------

   function Copy
     (Cloner : in Cloner_Class;
      Source : in Element;
      Parent : in Element) return Element
   is
      Result : Element := Nil_Element;
   begin
      if Assigned (Source) then
         Result := Clone (Cloner, Source, Parent);

         if Assigned (Result) then
            Copy (Source, Result, Cloner, Parent);
         end if;
      end if;

      return Result;
   end Copy;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy
     (Cloner : in Cloner_Class;
      Source : in Element;
      Parent : in Element) return Element
   is
      Result      : Element;
      The_Context : constant Context := Get_Context (Parent.all);
   begin
      Set_Check_Appropriate (The_Context.all, False);
      Result := Copy (Cloner, Source, Parent);
      Set_Check_Appropriate (The_Context.all, True);
      return Result;
   end Deep_Copy;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context (Item : in Element_Node'Class) return Context is
   begin
      return Enclosing_Context (Enclosing_Compilation_Unit (Item).all);
   end Get_Context;

   ------------------
   -- Get_Equality --
   ------------------

   generic
      type Element is private;
   package Generic_Get_Equality is
      function Is_Equal (Left, Right : Element) return Boolean
        renames "=";
   end Generic_Get_Equality;

   package Get_Equality is new Generic_Get_Equality (Element);

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (Left, Right : Element) return Boolean is
   begin
      return Get_Equality.Is_Equal (Left, Right);
   end Is_Equal;

   -------------
   -- Is_List --
   -------------

   function Is_List (Item : Element_Node) return Boolean is
   begin
      return False;
   end Is_List;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String (Item : Text_Position) return Wide_String is
      Line_Image   : constant Wide_String := Natural'Wide_Image (Item.Line);
      Column_Image : constant Wide_String := Natural'Wide_Image (Item.Column);
   begin
      return Line_Image (2 .. Line_Image'Last) & ":" &
        Column_Image (2 .. Column_Image'Last);
   end To_Wide_String;

   ---------------------------------
   -- Raise_Inappropriate_Element --
   ---------------------------------

   procedure Raise_Inappropriate_Element (Raiser : Wide_String := "") is
      Text : constant Wide_String := "Inappropriate element";
   begin
      if Raiser /= "" then
         Implementation.Set_Status
           (Errors.Value_Error, Text & " in " & Raiser);
      else
         Implementation.Set_Status
           (Errors.Value_Error, Text);
      end if;

      raise Exceptions.ASIS_Inappropriate_Element;
   end Raise_Inappropriate_Element;

   ---------------------------------
   -- Raise_Inappropriate_Element --
   ---------------------------------

   procedure Raise_Inappropriate_Element
     (The_Context : Context;
      Raiser      : Wide_String)
   is
   begin
      if Check_Appropriate (The_Context.all) then
         Raise_Inappropriate_Element (Raiser);
      end if;
   end Raise_Inappropriate_Element;

   ----------------------
   -- Set_Next_Element --
   ----------------------

   procedure Set_Next_Element
     (Item : in out Element_Node;
      Next : in     Element)
   is
   begin
      Raise_Inappropriate_Element ("Set_Next_Element");
   end Set_Next_Element;

   ---------------------
   -- Without_Pragmas --
   ---------------------

   function Without_Pragmas (List : Element_List) return Element_List is
      Result : Element_List (List'Range);
      Index  : List_Index := Result'First;
   begin
      for I in List'Range loop
         if Element_Kind (List (I).all) /= A_Pragma then
            Result (Index) := List (I);
            Index := Index + 1;
         end if;
      end loop;
      return Result (Result'First .. Index - 1);
   end Without_Pragmas;


   function Aborted_Tasks
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Aborted_Tasks");
      return Nil_Element_List;
   end Aborted_Tasks;

   function Accept_Body_Exception_Handlers
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Accept_Body_Exception_Handlers");
      return Nil_Element_List;
   end Accept_Body_Exception_Handlers;

   function Accept_Body_Statements
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Accept_Body_Statements");
      return Nil_Element_List;
   end Accept_Body_Statements;

   function Accept_Entry_Direct_Name
     (Element : Element_Node) return Asis.Name
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Accept_Entry_Direct_Name");
      return Nil_Element;
   end Accept_Entry_Direct_Name;

   function Accept_Entry_Index
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Accept_Entry_Index");
      return Nil_Element;
   end Accept_Entry_Index;

   function Accept_Parameters
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Accept_Parameters");
      return Nil_Element_List;
   end Accept_Parameters;

   function Access_To_Function_Result_Subtype
     (Element : Element_Node) return Asis.Definition
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Access_To_Function_Result_Subtype");
      return Nil_Element;
   end Access_To_Function_Result_Subtype;

   function Get_Access_To_Object_Definition
     (Element : Element_Node) return Asis.Subtype_Indication
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Get_Access_To_Object_Definition");
      return Nil_Element;
   end Get_Access_To_Object_Definition;

   function Access_To_Subprogram_Parameter_Profile
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Access_To_Subprogram_Parameter_Profile");
      return Nil_Element_List;
   end Access_To_Subprogram_Parameter_Profile;

   function Access_Type_Kind
     (Element : Element_Node) return Asis.Access_Type_Kinds
   is
   begin
      return Not_An_Access_Type_Definition;

   end Access_Type_Kind;

   function Actual_Parameter
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Actual_Parameter");
      return Nil_Element;
   end Actual_Parameter;

   function Allocator_Qualified_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Allocator_Qualified_Expression");
      return Nil_Element;
   end Allocator_Qualified_Expression;

   function Allocator_Subtype_Indication
     (Element : Element_Node) return Asis.Subtype_Indication
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Allocator_Subtype_Indication");
      return Nil_Element;
   end Allocator_Subtype_Indication;

   function Ancestor_Subtype_Indication
     (Element : Element_Node) return Asis.Subtype_Indication
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Ancestor_Subtype_Indication");
      return Nil_Element;
   end Ancestor_Subtype_Indication;

   function Anonymous_Access_To_Object_Subtype_Mark
     (Element : Element_Node) return Asis.Name
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Anonymous_Access_To_Object_Subtype_Mark");
      return Nil_Element;
   end Anonymous_Access_To_Object_Subtype_Mark;

   function Array_Component_Associations
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Array_Component_Associations");
      return Nil_Element_List;
   end Array_Component_Associations;

   function Array_Component_Choices
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Array_Component_Choices");
      return Nil_Element_List;
   end Array_Component_Choices;

   function Array_Component_Definition
     (Element : Element_Node) return Asis.Component_Definition
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Array_Component_Definition");
      return Nil_Element;
   end Array_Component_Definition;

   function Assignment_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Assignment_Expression");
      return Nil_Element;
   end Assignment_Expression;

   function Assignment_Variable_Name
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Assignment_Variable_Name");
      return Nil_Element;
   end Assignment_Variable_Name;

   function Attribute_Designator_Expressions
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Attribute_Designator_Expressions");
      return Nil_Element_List;
   end Attribute_Designator_Expressions;

   function Attribute_Designator_Identifier
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Attribute_Designator_Identifier");
      return Nil_Element;
   end Attribute_Designator_Identifier;

   function Attribute_Kind
     (Element : Element_Node) return Asis.Attribute_Kinds
   is
   begin
      return Not_An_Attribute;

   end Attribute_Kind;

   function Block_Declarative_Items
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Block_Declarative_Items");
      return Nil_Element_List;
   end Block_Declarative_Items;

   function Block_Exception_Handlers
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Block_Exception_Handlers");
      return Nil_Element_List;
   end Block_Exception_Handlers;

   function Block_Statements
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Block_Statements");
      return Nil_Element_List;
   end Block_Statements;

   function Body_Declarative_Items
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Body_Declarative_Items");
      return Nil_Element_List;
   end Body_Declarative_Items;

   function Body_Exception_Handlers
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Body_Exception_Handlers");
      return Nil_Element_List;
   end Body_Exception_Handlers;

   function Body_Statements
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Body_Statements");
      return Nil_Element_List;
   end Body_Statements;

   function Call_Statement_Parameters
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Call_Statement_Parameters");
      return Nil_Element_List;
   end Call_Statement_Parameters;

   function Called_Name
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Called_Name");
      return Nil_Element;
   end Called_Name;

   function Case_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Case_Expression");
      return Nil_Element;
   end Case_Expression;

   function Case_Statement_Alternative_Choices
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Case_Statement_Alternative_Choices");
      return Nil_Element_List;
   end Case_Statement_Alternative_Choices;

   function Choice_Parameter_Specification
     (Element : Element_Node) return Asis.Element
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Choice_Parameter_Specification");
      return Nil_Element;
   end Choice_Parameter_Specification;

   function Clause_Names
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Clause_Names");
      return Nil_Element_List;
   end Clause_Names;

   function Component_Clause_Position
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Component_Clause_Position");
      return Nil_Element;
   end Component_Clause_Position;

   function Component_Clause_Range
     (Element : Element_Node) return Asis.Discrete_Range
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Component_Clause_Range");
      return Nil_Element;
   end Component_Clause_Range;

   function Component_Clauses
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Component_Clauses");
      return Nil_Element_List;
   end Component_Clauses;

   function Component_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Component_Expression");
      return Nil_Element;
   end Component_Expression;

   function Component_Subtype_Indication
     (Element : Element_Node) return Asis.Subtype_Indication
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Component_Subtype_Indication");
      return Nil_Element;
   end Component_Subtype_Indication;

   function Condition_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Condition_Expression");
      return Nil_Element;
   end Condition_Expression;

   function Converted_Or_Qualified_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Converted_Or_Qualified_Expression");
      return Nil_Element;
   end Converted_Or_Qualified_Expression;

   function Converted_Or_Qualified_Subtype_Mark
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Converted_Or_Qualified_Subtype_Mark");
      return Nil_Element;
   end Converted_Or_Qualified_Subtype_Mark;

   function Corresponding_Base_Entity
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Base_Entity");
      return Nil_Element;
   end Corresponding_Base_Entity;

   function Corresponding_Body
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Body");
      return Nil_Element;
   end Corresponding_Body;

   function Corresponding_Body_Stub
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Body_Stub");
      return Nil_Element;
   end Corresponding_Body_Stub;

   function Corresponding_Called_Entity
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Called_Entity");
      return Nil_Element;
   end Corresponding_Called_Entity;

   function Corresponding_Called_Function
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Called_Function");
      return Nil_Element;
   end Corresponding_Called_Function;

   function Corresponding_Constant_Declaration
     (Element : Element_Node) return Asis.Element
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Constant_Declaration");
      return Nil_Element;
   end Corresponding_Constant_Declaration;

   function Corresponding_Declaration
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Declaration");
      return Nil_Element;
   end Corresponding_Declaration;

   function Corresponding_Destination_Statement
     (Element : Element_Node) return Asis.Statement
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Destination_Statement");
      return Nil_Element;
   end Corresponding_Destination_Statement;

   function Corresponding_Entry
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Entry");
      return Nil_Element;
   end Corresponding_Entry;

   function Corresponding_Equality_Operator
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Equality_Operator");
      return Nil_Element;
   end Corresponding_Equality_Operator;

   function Corresponding_Expression_Type
     (Element : Element_Node) return Asis.Element
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Expression_Type");
      return Nil_Element;
   end Corresponding_Expression_Type;

   function Corresponding_First_Subtype
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_First_Subtype");
      return Nil_Element;
   end Corresponding_First_Subtype;

   function Corresponding_Generic_Element
     (Element : Element_Node) return Asis.Defining_Name
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Generic_Element");
      return Nil_Element;
   end Corresponding_Generic_Element;

   function Corresponding_Last_Constraint
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Last_Constraint");
      return Nil_Element;
   end Corresponding_Last_Constraint;

   function Corresponding_Last_Subtype
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Last_Subtype");
      return Nil_Element;
   end Corresponding_Last_Subtype;

   function Corresponding_Loop_Exited
     (Element : Element_Node) return Asis.Statement
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Loop_Exited");
      return Nil_Element;
   end Corresponding_Loop_Exited;

   function Corresponding_Name_Declaration
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Name_Declaration");
      return Nil_Element;
   end Corresponding_Name_Declaration;

   function Corresponding_Name_Definition_List
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Name_Definition_List");
      return Nil_Element_List;
   end Corresponding_Name_Definition_List;

   function Corresponding_Parent_Subtype
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Parent_Subtype");
      return Nil_Element;
   end Corresponding_Parent_Subtype;

   function Corresponding_Pragmas
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Pragmas");
      return Nil_Element_List;
   end Corresponding_Pragmas;

   function Corresponding_Representation_Clauses
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Representation_Clauses");
      return Nil_Element_List;
   end Corresponding_Representation_Clauses;

   function Corresponding_Root_Type
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Root_Type");
      return Nil_Element;
   end Corresponding_Root_Type;

   function Corresponding_Subprogram_Derivation
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Subprogram_Derivation");
      return Nil_Element;
   end Corresponding_Subprogram_Derivation;

   function Corresponding_Subunit
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Subunit");
      return Nil_Element;
   end Corresponding_Subunit;

   function Corresponding_Type
     (Element : Element_Node) return Asis.Type_Definition
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Type");
      return Nil_Element;
   end Corresponding_Type;

   function Corresponding_Type_Declaration
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Type_Declaration");
      return Nil_Element;
   end Corresponding_Type_Declaration;

   function Corresponding_Type_Operators
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Type_Operators");
      return Nil_Element_List;
   end Corresponding_Type_Operators;

   function Corresponding_Type_Structure
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Corresponding_Type_Structure");
      return Nil_Element;
   end Corresponding_Type_Structure;

   function Declaration_Origin
     (Element : Element_Node) return Asis.Declaration_Origins
   is
   begin
      return Not_A_Declaration_Origin;

   end Declaration_Origin;

   function Default_Kind
     (Element : Element_Node) return Asis.Subprogram_Default_Kinds
   is
   begin
      return Not_A_Default;

   end Default_Kind;

   function Defining_Name_Image
     (Element : Element_Node) return Wide_String
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Defining_Name_Image");
      return "";
   end Defining_Name_Image;

   function Defining_Prefix
     (Element : Element_Node) return Asis.Name
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Defining_Prefix");
      return Nil_Element;
   end Defining_Prefix;

   function Defining_Selector
     (Element : Element_Node) return Asis.Defining_Name
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Defining_Selector");
      return Nil_Element;
   end Defining_Selector;

   function Delay_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Delay_Expression");
      return Nil_Element;
   end Delay_Expression;

   function Delta_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Delta_Expression");
      return Nil_Element;
   end Delta_Expression;

   function Digits_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Digits_Expression");
      return Nil_Element;
   end Digits_Expression;

   function Discrete_Ranges
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Discrete_Ranges");
      return Nil_Element_List;
   end Discrete_Ranges;

   function Discrete_Subtype_Definitions
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Discrete_Subtype_Definitions");
      return Nil_Element_List;
   end Discrete_Subtype_Definitions;

   function Discriminant_Associations
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Discriminant_Associations");
      return Nil_Element_List;
   end Discriminant_Associations;

   function Discriminant_Direct_Name
     (Element : Element_Node) return Asis.Name
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Discriminant_Direct_Name");
      return Nil_Element;
   end Discriminant_Direct_Name;

   function Discriminant_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Discriminant_Expression");
      return Nil_Element;
   end Discriminant_Expression;

   function Discriminant_Part
     (Element : Element_Node) return Asis.Definition
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Discriminant_Part");
      return Nil_Element;
   end Discriminant_Part;

   function Discriminant_Selector_Name
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Discriminant_Selector_Name");
      return Nil_Element;
   end Discriminant_Selector_Name;

   function Discriminant_Selector_Names
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Discriminant_Selector_Names");
      return Nil_Element_List;
   end Discriminant_Selector_Names;

   function Discriminants
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Discriminants");
      return Nil_Element_List;
   end Discriminants;

   function Enclosing_Compilation_Unit
     (Element : Element_Node) return Asis.Compilation_Unit
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Enclosing_Compilation_Unit");
      return Nil_Compilation_Unit;
   end Enclosing_Compilation_Unit;

   function Enclosing_Element
     (Element : Element_Node) return Asis.Element
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Enclosing_Element");
      return Nil_Element;
   end Enclosing_Element;

   function End_Position
     (Element : Element_Node) return Asis.Text_Position
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "End_Position");
      return Nil_Text_Position;
   end End_Position;

   function Entry_Barrier
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Entry_Barrier");
      return Nil_Element;
   end Entry_Barrier;

   function Entry_Family_Definition
     (Element : Element_Node) return Asis.Discrete_Subtype_Definition
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Entry_Family_Definition");
      return Nil_Element;
   end Entry_Family_Definition;

   function Entry_Index_Specification
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Entry_Index_Specification");
      return Nil_Element;
   end Entry_Index_Specification;

   function Enumeration_Literal_Declarations
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Enumeration_Literal_Declarations");
      return Nil_Element_List;
   end Enumeration_Literal_Declarations;

   function Exception_Choices
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Exception_Choices");
      return Nil_Element_List;
   end Exception_Choices;

   function Exception_Handlers
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Exception_Handlers");
      return Nil_Element_List;
   end Exception_Handlers;

   function Exit_Condition
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Exit_Condition");
      return Nil_Element;
   end Exit_Condition;

   function Exit_Loop_Name
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Exit_Loop_Name");
      return Nil_Element;
   end Exit_Loop_Name;

   function Expression_Parenthesized
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Expression_Parenthesized");
      return Nil_Element;
   end Expression_Parenthesized;

   function Extended_Return_Exception_Handlers
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Extended_Return_Exception_Handlers");
      return Nil_Element_List;
   end Extended_Return_Exception_Handlers;

   function Extended_Return_Statements
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Extended_Return_Statements");
      return Nil_Element_List;
   end Extended_Return_Statements;

   function Extension_Aggregate_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Extension_Aggregate_Expression");
      return Nil_Element;
   end Extension_Aggregate_Expression;

   function Formal_Parameter
     (Element : Element_Node) return Asis.Identifier
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Formal_Parameter");
      return Nil_Element;
   end Formal_Parameter;

   function Formal_Subprogram_Default
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Formal_Subprogram_Default");
      return Nil_Element;
   end Formal_Subprogram_Default;

   function Function_Call_Parameters
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Function_Call_Parameters");
      return Nil_Element_List;
   end Function_Call_Parameters;

   function Generic_Actual_Part
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Generic_Actual_Part");
      return Nil_Element_List;
   end Generic_Actual_Part;

   function Generic_Formal_Part
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Generic_Formal_Part");
      return Nil_Element_List;
   end Generic_Formal_Part;

   function Generic_Unit_Name
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Generic_Unit_Name");
      return Nil_Element;
   end Generic_Unit_Name;

   function Goto_Label
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Goto_Label");
      return Nil_Element;
   end Goto_Label;

   function Guard
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Guard");
      return Nil_Element;
   end Guard;

   function Handler_Statements
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Handler_Statements");
      return Nil_Element_List;
   end Handler_Statements;

   function Has_Abstract
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Has_Abstract;

   function Has_Limited
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Has_Limited;

   function Has_Null_Exclusion
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Has_Null_Exclusion;

   function Has_Private
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Has_Private;

   function Has_Protected
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Has_Protected;

   function Has_Synchronized
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Has_Synchronized;

   function Has_Tagged
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Has_Tagged;

   function Has_Task
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Has_Task;

   function Hash
     (Element : Element_Node) return Asis.ASIS_Integer
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Hash");
      return 0;
   end Hash;

   function Implicit_Components
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Implicit_Components");
      return Nil_Element_List;
   end Implicit_Components;

   function Implicit_Inherited_Declarations
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Implicit_Inherited_Declarations");
      return Nil_Element_List;
   end Implicit_Inherited_Declarations;

   function Implicit_Inherited_Subprograms
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Implicit_Inherited_Subprograms");
      return Nil_Element_List;
   end Implicit_Inherited_Subprograms;

   function Index_Expressions
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Index_Expressions");
      return Nil_Element_List;
   end Index_Expressions;

   function Index_Subtype_Definitions
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Index_Subtype_Definitions");
      return Nil_Element_List;
   end Index_Subtype_Definitions;

   function Initialization_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Initialization_Expression");
      return Nil_Element;
   end Initialization_Expression;

   function Integer_Constraint
     (Element : Element_Node) return Asis.Range_Constraint
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Integer_Constraint");
      return Nil_Element;
   end Integer_Constraint;

   function Interface_Kind
     (Element : Element_Node) return Asis.Interface_Kinds
   is
   begin
      return Not_An_Interface;

   end Interface_Kind;

   function Is_Call_On_Dispatching_Operation
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Call_On_Dispatching_Operation;

   function Is_Declare_Block
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Declare_Block;

   function Is_Defaulted_Association
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Defaulted_Association;

   function Is_Dispatching_Call
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Dispatching_Call;

   function Is_Dispatching_Operation
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Dispatching_Operation;

   function Is_Name_Repeated
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Name_Repeated;

   function Is_Normalized
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Normalized;

   function Is_Null_Procedure
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Null_Procedure;

   function Is_Part_Of_Implicit
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Part_Of_Implicit;

   function Is_Part_Of_Inherited
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Part_Of_Inherited;

   function Is_Part_Of_Instance
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Part_Of_Instance;

   function Is_Prefix_Call
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Prefix_Call;

   function Is_Private_Present
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Private_Present;

   function Is_Task_Definition_Present
     (Element : Element_Node) return Boolean
   is
   begin
      return False;

   end Is_Task_Definition_Present;

   function Label_Names
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Label_Names");
      return Nil_Element_List;
   end Label_Names;

   function Loop_Parameter_Specification
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Loop_Parameter_Specification");
      return Nil_Element;
   end Loop_Parameter_Specification;

   function Loop_Statements
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Loop_Statements");
      return Nil_Element_List;
   end Loop_Statements;

   function Lower_Bound
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Lower_Bound");
      return Nil_Element;
   end Lower_Bound;

   function Membership_Test_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Membership_Test_Expression");
      return Nil_Element;
   end Membership_Test_Expression;

   function Membership_Test_Range
     (Element : Element_Node) return Asis.Range_Constraint
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Membership_Test_Range");
      return Nil_Element;
   end Membership_Test_Range;

   function Membership_Test_Subtype_Mark
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Membership_Test_Subtype_Mark");
      return Nil_Element;
   end Membership_Test_Subtype_Mark;

   function Mod_Clause_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Mod_Clause_Expression");
      return Nil_Element;
   end Mod_Clause_Expression;

   function Mod_Static_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Mod_Static_Expression");
      return Nil_Element;
   end Mod_Static_Expression;

   function Mode_Kind
     (Element : Element_Node) return Asis.Mode_Kinds
   is
   begin
      return Not_A_Mode;

   end Mode_Kind;

   function Name_Image
     (Element : Element_Node) return Wide_String
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Name_Image");
      return "";
   end Name_Image;

   function Names
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Names");
      return Nil_Element_List;
   end Names;

   function Next_Element
     (Element : Element_Node) return Asis.Element
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Next_Element");
      return Nil_Element;
   end Next_Element;

   function Normalized_Call_Statement_Parameters
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Normalized_Call_Statement_Parameters");
      return Nil_Element_List;
   end Normalized_Call_Statement_Parameters;

   function Normalized_Discriminant_Associations
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Normalized_Discriminant_Associations");
      return Nil_Element_List;
   end Normalized_Discriminant_Associations;

   function Normalized_Function_Call_Parameters
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Normalized_Function_Call_Parameters");
      return Nil_Element_List;
   end Normalized_Function_Call_Parameters;

   function Normalized_Generic_Actual_Part
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Normalized_Generic_Actual_Part");
      return Nil_Element_List;
   end Normalized_Generic_Actual_Part;

   function Normalized_Record_Component_Associations
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Normalized_Record_Component_Associations");
      return Nil_Element_List;
   end Normalized_Record_Component_Associations;

   function Object_Declaration_Subtype
     (Element : Element_Node) return Asis.Definition
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Object_Declaration_Subtype");
      return Nil_Element;
   end Object_Declaration_Subtype;

   function Operator_Kind
     (Element : Element_Node) return Asis.Operator_Kinds
   is
   begin
      return Not_An_Operator;

   end Operator_Kind;

   function Overriding_Indicator_Kind
     (Element : Element_Node) return Asis.Overriding_Indicator_Kinds
   is
   begin
      return Not_An_Overriding_Indicator;

   end Overriding_Indicator_Kind;

   function Parameter_Profile
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Parameter_Profile");
      return Nil_Element_List;
   end Parameter_Profile;

   function Parent_Subtype_Indication
     (Element : Element_Node) return Asis.Subtype_Indication
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Parent_Subtype_Indication");
      return Nil_Element;
   end Parent_Subtype_Indication;

   function Position_Number_Image
     (Element : Element_Node) return Wide_String
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Position_Number_Image");
      return "";
   end Position_Number_Image;

   function Pragma_Argument_Associations
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Pragma_Argument_Associations");
      return Nil_Element_List;
   end Pragma_Argument_Associations;

   function Pragma_Kind
     (Element : Element_Node) return Asis.Pragma_Kinds
   is
   begin
      return Not_A_Pragma;

   end Pragma_Kind;

   function Pragma_Name_Image
     (Element : Element_Node) return Wide_String
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Pragma_Name_Image");
      return "";
   end Pragma_Name_Image;

   function Pragmas
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Pragmas");
      return Nil_Element_List;
   end Pragmas;

   function Prefix
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Prefix");
      return Nil_Element;
   end Prefix;

   function Private_Part_Declarative_Items
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Private_Part_Declarative_Items");
      return Nil_Element_List;
   end Private_Part_Declarative_Items;

   function Private_Part_Items
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Private_Part_Items");
      return Nil_Element_List;
   end Private_Part_Items;

   function Profile
     (Element : Element_Node) return Asis.Element
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Profile");
      return Nil_Element;
   end Profile;

   function Progenitor_List
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Progenitor_List");
      return Nil_Element_List;
   end Progenitor_List;

   function Protected_Operation_Items
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Protected_Operation_Items");
      return Nil_Element_List;
   end Protected_Operation_Items;

   function Qualified_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Qualified_Expression");
      return Nil_Element;
   end Qualified_Expression;

   function Raise_Statement_Message
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Raise_Statement_Message");
      return Nil_Element;
   end Raise_Statement_Message;

   function Raised_Exception
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Raised_Exception");
      return Nil_Element;
   end Raised_Exception;

   function Range_Attribute
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Range_Attribute");
      return Nil_Element;
   end Range_Attribute;

   function Raw_Image
     (Element : Element_Node) return Gela_String
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Raw_Image");
      return Raw_Image (Element);
   end Raw_Image;

   function Real_Range_Constraint
     (Element : Element_Node) return Asis.Range_Constraint
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Real_Range_Constraint");
      return Nil_Element;
   end Real_Range_Constraint;

   function Record_Component_Associations
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Record_Component_Associations");
      return Nil_Element_List;
   end Record_Component_Associations;

   function Record_Component_Choice
     (Element : Element_Node) return Asis.Defining_Name
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Record_Component_Choice");
      return Nil_Element;
   end Record_Component_Choice;

   function Record_Component_Choices
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Record_Component_Choices");
      return Nil_Element_List;
   end Record_Component_Choices;

   function Record_Components
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Record_Components");
      return Nil_Element_List;
   end Record_Components;

   function Get_Record_Definition
     (Element : Element_Node) return Asis.Definition
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Get_Record_Definition");
      return Nil_Element;
   end Get_Record_Definition;

   function References
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "References");
      return Nil_Element_List;
   end References;

   function Renamed_Entity
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Renamed_Entity");
      return Nil_Element;
   end Renamed_Entity;

   function Representation_Clause_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Representation_Clause_Expression");
      return Nil_Element;
   end Representation_Clause_Expression;

   function Representation_Clause_Name
     (Element : Element_Node) return Asis.Name
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Representation_Clause_Name");
      return Nil_Element;
   end Representation_Clause_Name;

   function Representation_Value_Image
     (Element : Element_Node) return Wide_String
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Representation_Value_Image");
      return "";
   end Representation_Value_Image;

   function Requeue_Entry_Name
     (Element : Element_Node) return Asis.Name
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Requeue_Entry_Name");
      return Nil_Element;
   end Requeue_Entry_Name;

   function Result_Subtype
     (Element : Element_Node) return Asis.Definition
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Result_Subtype");
      return Nil_Element;
   end Result_Subtype;

   function Return_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Return_Expression");
      return Nil_Element;
   end Return_Expression;

   function Return_Object_Specification
     (Element : Element_Node) return Asis.Declaration
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Return_Object_Specification");
      return Nil_Element;
   end Return_Object_Specification;

   function Root_Type_Kind
     (Element : Element_Node) return Asis.Root_Type_Kinds
   is
   begin
      return Not_A_Root_Type_Definition;

   end Root_Type_Kind;

   function Selector
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Selector");
      return Nil_Element;
   end Selector;

   function Sequence_Of_Statements
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Sequence_Of_Statements");
      return Nil_Element_List;
   end Sequence_Of_Statements;

   function Short_Circuit_Operation_Left_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Short_Circuit_Operation_Left_Expression");
      return Nil_Element;
   end Short_Circuit_Operation_Left_Expression;

   function Short_Circuit_Operation_Right_Expression
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Short_Circuit_Operation_Right_Expression");
      return Nil_Element;
   end Short_Circuit_Operation_Right_Expression;

   function Slice_Range
     (Element : Element_Node) return Asis.Discrete_Range
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Slice_Range");
      return Nil_Element;
   end Slice_Range;

   function Specification_Subtype_Definition
     (Element : Element_Node) return Asis.Discrete_Subtype_Definition
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Specification_Subtype_Definition");
      return Nil_Element;
   end Specification_Subtype_Definition;

   function Start_Position
     (Element : Element_Node) return Asis.Text_Position
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Start_Position");
      return Nil_Text_Position;
   end Start_Position;

   function Statement_Identifier
     (Element : Element_Node) return Asis.Defining_Name
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Statement_Identifier");
      return Nil_Element;
   end Statement_Identifier;

   function Statement_Paths
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Statement_Paths");
      return Nil_Element_List;
   end Statement_Paths;

   function Subtype_Constraint
     (Element : Element_Node) return Asis.Constraint
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Subtype_Constraint");
      return Nil_Element;
   end Subtype_Constraint;

   function Get_Subtype_Mark
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Get_Subtype_Mark");
      return Nil_Element;
   end Get_Subtype_Mark;

   function Trait_Kind
     (Element : Element_Node) return Asis.Trait_Kinds
   is
   begin
      return Not_A_Trait;

   end Trait_Kind;

   function Type_Declaration_View
     (Element : Element_Node) return Asis.Definition
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Type_Declaration_View");
      return Nil_Element;
   end Type_Declaration_View;

   function Upper_Bound
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Upper_Bound");
      return Nil_Element;
   end Upper_Bound;

   function Value_Image
     (Element : Element_Node) return Wide_String
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Value_Image");
      return "";
   end Value_Image;

   function Variant_Choices
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Variant_Choices");
      return Nil_Element_List;
   end Variant_Choices;

   function Variants
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Variants");
      return Nil_Element_List;
   end Variants;

   function Visible_Part_Declarative_Items
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Visible_Part_Declarative_Items");
      return Nil_Element_List;
   end Visible_Part_Declarative_Items;

   function Visible_Part_Items
     (Element : Element_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "Visible_Part_Items");
      return Nil_Element_List;
   end Visible_Part_Items;

   function While_Condition
     (Element : Element_Node) return Asis.Expression
   is
   begin
      Raise_Inappropriate_Element
        (Get_Context (Element), "While_Condition");
      return Nil_Element;
   end While_Condition;

   function Access_Definition_Kind (Element : Element_Node)
      return Asis.Access_Definition_Kinds is
   begin
      return Not_An_Access_Definition;
   end Access_Definition_Kind;

   function Association_Kind (Element : Element_Node)
      return Asis.Association_Kinds is
   begin
      return Not_An_Association;
   end Association_Kind;

   function Clause_Kind (Element : Element_Node)
      return Asis.Clause_Kinds is
   begin
      return Not_A_Clause;
   end Clause_Kind;

   function Constraint_Kind (Element : Element_Node)
      return Asis.Constraint_Kinds is
   begin
      return Not_A_Constraint;
   end Constraint_Kind;

   function Declaration_Kind (Element : Element_Node)
      return Asis.Declaration_Kinds is
   begin
      return Not_A_Declaration;
   end Declaration_Kind;

   function Defining_Name_Kind (Element : Element_Node)
      return Asis.Defining_Name_Kinds is
   begin
      return Not_A_Defining_Name;
   end Defining_Name_Kind;

   function Definition_Kind (Element : Element_Node)
      return Asis.Definition_Kinds is
   begin
      return Not_A_Definition;
   end Definition_Kind;

   function Discrete_Range_Kind (Element : Element_Node)
      return Asis.Discrete_Range_Kinds is
   begin
      return Not_A_Discrete_Range;
   end Discrete_Range_Kind;

   function Element_Kind (Element : Element_Node)
      return Asis.Element_Kinds is
   begin
      return Not_An_Element;
   end Element_Kind;

   function Expression_Kind (Element : Element_Node)
      return Asis.Expression_Kinds is
   begin
      return Not_An_Expression;
   end Expression_Kind;

   function Formal_Type_Definition_Kind (Element : Element_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return Not_A_Formal_Type_Definition;
   end Formal_Type_Definition_Kind;

   function Path_Kind (Element : Element_Node)
      return Asis.Path_Kinds is
   begin
      return Not_A_Path;
   end Path_Kind;

   function Representation_Clause_Kind (Element : Element_Node)
      return Asis.Representation_Clause_Kinds is
   begin
      return Not_A_Representation_Clause;
   end Representation_Clause_Kind;

   function Statement_Kind (Element : Element_Node)
      return Asis.Statement_Kinds is
   begin
      return Not_A_Statement;
   end Statement_Kind;

   function Type_Definition_Kind (Element : Element_Node)
      return Asis.Type_Kinds is
   begin
      return Not_A_Type_Definition;
   end Type_Definition_Kind;


end Asis;


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
