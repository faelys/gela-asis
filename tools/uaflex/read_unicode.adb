------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $


--  Run this to generate symbols-unicode.adb file.
--  It requires UnicodeData.txt and PropertyValueAliases.txt from Unicode.org

with Ada.Text_IO;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
--with Ada.Text_IO.Integer_IO;
with Ada.Containers.Vectors;

procedure Read_Unicode is
   use Ada.Text_IO;
   use Ada.Strings;

   package U renames Ada.Strings.Unbounded;

   type Code_Point is range 0 .. 16#10FFFF#;

   package Code_Point_IO is new Ada.Text_IO.Integer_IO (Code_Point);


   Semicolumn : Maps.Character_Set := Maps.To_Set (";");

   type Category is record
      Alias : String (1 .. 2);
      Name  : U.Unbounded_String;
      Count : Natural := 0;
   end record;

   type Categories is array (Positive range <>) of Category;

   type Unicode_Data_Item is record
      From, To : Code_Point;
      Category : String (1 .. 2);
   end record;

   package Vectors is new Ada.Containers.Vectors
     (Positive, Unicode_Data_Item);

   function Read_Categories return Categories;

   procedure Read_Characters (Data : out Vectors.Vector);

   procedure Get_Field
     (Line : in     String;
      From :    out Positive;
      To   :    out Natural;
      Last : in out Natural);

   procedure P (Text : String);

   function Is_Range (Text : String) return Boolean;
   function Is_Last_Name (Name : String) return Boolean;

   function To_Code_Point (Text : String) return Code_Point;

   procedure To_Code_Points
     (Text : String;
      From : out Code_Point;
      To   : out Code_Point);

   function Less_Category (Left, Right : Unicode_Data_Item) return Boolean;

   package Category_Sorting is new Vectors.Generic_Sorting (Less_Category);

   function "+" (Value : Natural) return String;
   function "+" (Value : Code_Point) return String;

   ---------
   -- "+" --
   ---------

   function "+" (Value : Natural) return String is
      Image : constant String := Natural'Image (Value);
   begin
      return Image (2 .. Image'Last);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Value : Code_Point) return String is
      Text : String (1 .. 10);
   begin
      Code_Point_IO.Put (Text, Value, Base => 16);
      return Text;
   end "+";

   ---------------
   -- Get_Field --
   ---------------

   procedure Get_Field
     (Line : in     String;
      From :    out Positive;
      To   :    out Natural;
      Last : in out Natural)
   is
      use Ada.Strings.Fixed;
   begin
      Find_Token (Line (Last .. Line'Last), Semicolumn, Outside, From, To);

      if To = 0 then
         Last := 0;
      else
         Last := To + 1;
         From := Index_Non_Blank (Line (From .. To));
         To   := Index_Non_Blank (Line (From .. To), Backward);
      end if;
   end Get_Field;

   ------------------
   -- Is_Last_Name --
   ------------------

   function Is_Last_Name (Name : String) return Boolean is
   begin
      return Fixed.Index (Name, ", Last>") > 0;
   end Is_Last_Name;

   --------------
   -- Is_Range --
   --------------

   function Is_Range (Text : String) return Boolean is
   begin
      return Fixed.Index (Text, "..") > 0;
   end Is_Range;

   -------------------
   -- Less_Category --
   -------------------

   function Less_Category (Left, Right : Unicode_Data_Item) return Boolean is
   begin
      return Left.Category < Right.Category or else
        (Left.Category = Right.Category and Left.From < Right.From);
   end Less_Category;

   -------
   -- P --
   -------

   procedure P (Text : String) is
   begin
      Put_Line (Text);
   end P;

   --------------------
   -- Print_Function --
   --------------------

   procedure Print_Function (List : Categories) is
   begin
      P ("   function General_Category (Name : String) return Symbol_Set is");
      P ("      type General_Category is");
      for J in List'Range loop
         if J = 1 then
            P ("        (" & U.To_String (List (J).Name) & ",");
         elsif J = List'Last then
            P ("         " & U.To_String (List (J).Name) & ");");
         else
            P ("         " & U.To_String (List (J).Name) & ",");
         end if;
      end loop;

      P ("");
      P ("      Category : General_Category_Alias;");
      P ("      GC       : General_Category;");
      P ("");
      P ("   begin");
      P ("      if Name'Length = 2 then");
      P ("         Category := General_Category_Alias'Value (Name);");
      P ("      else");
      P ("         GC := General_Category'Value (Name);");
      P ("         Category := General_Category_Alias'Val " &
         "(General_Category'Pos (GC));");
      P ("      end if;");
      P ("      return Values (Category);");
      P ("   end General_Category;");
      P ("");
      P ("end Symbols.Unicode;");
   end Print_Function;

   ------------------
   -- Print_Header --
   ------------------

   procedure Print_Header is
   begin
      P ("--  This file is generated by read_unicode.adb. DON'T EDIT!");
      P ("");
      P ("package body Symbols.Unicode is");
      P ("");
      P ("   type General_Category_Alias is");
   end Print_Header;

   ------------------
   -- Print_Values --
   ------------------

   procedure Print_Values (List : Categories) is
   begin
      P ("   Values : constant array (General_Category_Alias) " &
         "of Symbol_Set :=");

      for J in List'Range loop
         if J = 1 then
            P ("        (" & List (J).Alias &
               " => (F.Controlled with " &
               List (J).Alias & "_Node'Access),");
         elsif J = List'Last then
            P ("         " & List (J).Alias &
               " => (F.Controlled with " &
               List (J).Alias & "_Node'Access));");
         else
            P ("         " & List (J).Alias &
               " => (F.Controlled with " &
               List (J).Alias & "_Node'Access),");
         end if;
      end loop;

      P ("");
   end Print_Values;

   ---------------------
   -- Read_Categories --
   ---------------------

   function Read_Categories return Categories is
      Input  : File_Type;
      Result : Categories (1 .. 30);
      Last   : Natural := 0;
   begin
      Open (Input, In_File, "PropertyValueAliases.txt");

      while not End_Of_File (Input) loop
         declare
            Line : constant String := Get_Line (Input);
            From : Positive;
            To   : Natural;
            Pos  : Natural := Line'First;
         begin
            Get_Field (Line, From, To, Pos);

            if Pos > 0 and then Line (From .. To) = "gc" then
               Get_Field (Line, From, To, Pos);

               if To - From = 1 and then Line (From .. To) /= "LC" then
                  Last := Last + 1;
                  Result (Last).Alias := Line (From .. To);
                  Get_Field (Line, From, To, Pos);
                  Result (Last).Name :=
                    U.To_Unbounded_String (Line (From .. To));
               end if;
            end if;
         end;
      end loop;

      Close (Input);

      return Result (1 .. Last);
   end Read_Categories;

   procedure Read_Characters (Data : out Vectors.Vector) is
      Input : File_Type;
   begin
      Open (Input, In_File, "UnicodeData.txt");
      Vectors.Reserve_Capacity (Data, 18_000);

      while not End_Of_File (Input) loop
         declare
            Item  : Unicode_Data_Item;
            Text  : constant String := Get_Line (Input);
            From  : Positive;
            To    : Natural;
            From2 : Positive;
            To2   : Natural;
            Last  : Natural := Text'First;
         begin
            Get_Field (Text, From, To, Last);

            if Last > 0 then
               Get_Field (Text, From2, To2, Last);

               if Is_Range (Text (From .. To)) then
                  To_Code_Points (Text (From .. To), Item.From, Item.To);
               elsif Is_Last_Name (Text (From2 .. To2)) then
                  Item := Vectors.Last_Element (Data);
                  Vectors.Delete_Last (Data);
                  Item.To := To_Code_Point (Text (From .. To));
               else
                  Item.From := To_Code_Point (Text (From .. To));
                  Item.To   := Item.From;
               end if;

               Get_Field (Text, From2, To2, Last);

               Item.Category := Text (From2 .. To2);
               Vectors.Append (Data, Item);
            end if;
         end;
      end loop;

      Close (Input);
   end Read_Characters;

   -------------------
   -- To_Code_Point --
   -------------------

   function To_Code_Point (Text : String) return Code_Point is
      Image : constant String := "16#" & Text & "#";
   begin
      return Code_Point'Value (Image);
   end To_Code_Point;

   --------------------
   -- To_Code_Points --
   --------------------

   procedure To_Code_Points
     (Text : String;
      From : out Code_Point;
      To   : out Code_Point)
   is
      Pos : constant Positive := Fixed.Index (Text, "..");
   begin
      From := To_Code_Point (Text (Text'First .. Pos - 1));
      To   := To_Code_Point (Text (Pos + 2 .. Text'Last));
   end To_Code_Points;

   List : Categories := Read_Categories;
   Data : Vectors.Vector;

begin
   Print_Header;

   for J in List'Range loop
      if J = 1 then
         P ("     (" & List (J).Alias & ",");
      elsif J = List'Last then
         P ("      " & List (J).Alias & ");");
      else
         P ("      " & List (J).Alias & ",");
      end if;
   end loop;

   P ("");

   Read_Characters (Data);
   Category_Sorting.Sort (Data);

   declare
      Current : String (1 .. 2) := "  ";
      Index   : Positive;
      Last    : Code_Point;

      procedure Count_Category (Position : Vectors.Cursor) is
         Item : constant Unicode_Data_Item := Vectors.Element (Position);
      begin
         if Current = Item.Category then
            if Item.From /= Last + 1 then
               List (Index).Count := List (Index).Count + 1;
            end if;

            Last := Item.To;
         else
            Current := Item.Category;

            for J in List'Range loop
               if List (J).Alias = Item.Category then
                  Index := J;
                  exit;
               end if;
            end loop;

            List (Index).Count := 1;
            Last := Item.To;
         end if;
      end Count_Category;
   begin
      Vectors.Iterate (Data, Count_Category'Access);
   end;

   declare
      Pos      : Vectors.Cursor := Vectors.First (Data);
      From, To : Code_Point;
      First    : Boolean;
   begin
      for J in List'Range loop
         if List (J).Count > 0 then
            P ("   " & List (J).Alias & "_Node : aliased Set_Node :=");
            P ("     (" & (+List (J).Count) & ", 2, (");

            First := True;

            while Vectors.Has_Element (Pos) loop
               declare
                  Item : Unicode_Data_Item := Vectors.Element (Pos);
               begin
                  exit when Item.Category /= List (J).Alias;

                  if First then
                     From  := Item.From;
                     To    := Item.To;
                     First := False;
                  elsif To + 1 = Item.From then
                     To := Item.To;
                  else
                     P ("      (" & (+From) & ", " & (+To) & "),");

                     From := Item.From;
                     To   := Item.To;
                  end if;
               end;

               Pos := Vectors.Next (Pos);
            end loop;

            if List (J).Count = 1 then
               P ("      1 => (" & (+From) & ", " & (+To) & ")));");
            else
               P ("      (" & (+From) & ", " & (+To) & ")));");
            end if;

            P ("");
         end if;
      end loop;
   end;


   P ("   Cn_Node : aliased Set_Node :=");
   P ("     (0, 2, (others => (Not_A_Symbol, Not_A_Symbol)));");
   P ("");

   Print_Values (List);
   Print_Function (List);

end Read_Unicode;


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
