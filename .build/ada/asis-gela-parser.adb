

with Asis.Gela.Scanners;
with Asis.Gela.Scanner_Tables;
with Gela.Character_Class_Buffers;
with Gela.Classificators;
with Asis.Gela.Classificators_Create;

with Ada.Text_IO;
with Ada.Characters.Handling;

with Asis.Gela.Elements;               use Asis.Gela.Elements;
with Asis.Gela.Elements.Assoc;         use Asis.Gela.Elements.Assoc;
with Asis.Gela.Elements.Clause;        use Asis.Gela.Elements.Clause;
with Asis.Gela.Elements.Clause.Rep;    use Asis.Gela.Elements.Clause.Rep;
with Asis.Gela.Elements.Decl;          use Asis.Gela.Elements.Decl;
with Asis.Gela.Elements.Defs;          use Asis.Gela.Elements.Defs;
with Asis.Gela.Elements.Defs.Accs;     use Asis.Gela.Elements.Defs.Accs;
with Asis.Gela.Elements.Defs.Const;    use Asis.Gela.Elements.Defs.Const;
with Asis.Gela.Elements.Defs.Formal;   use Asis.Gela.Elements.Defs.Formal;
with Asis.Gela.Elements.Defs.Types;    use Asis.Gela.Elements.Defs.Types;
with Asis.Gela.Elements.Defs.Rng;      use Asis.Gela.Elements.Defs.Rng;
with Asis.Gela.Elements.Defs.Sub;      use Asis.Gela.Elements.Defs.Sub;
with Asis.Gela.Elements.Expr;          use Asis.Gela.Elements.Expr;
with Asis.Gela.Elements.Def_Names;     use Asis.Gela.Elements.Def_Names;
with Asis.Gela.Elements.Pathes;        use Asis.Gela.Elements.Pathes;
with Asis.Gela.Elements.Stmt;          use Asis.Gela.Elements.Stmt;
with Asis.Gela.Elements.Helpers;       use Asis.Gela.Elements.Helpers;
with Asis.Gela.Units;                  use Asis.Gela.Units;
with Asis.Gela.Base_Lists;             use Asis.Gela.Base_Lists;
with Asis.Gela.Lists;                  use Asis.Gela.Lists;
with Asis.Gela.Library;

with Asis.Gela.Parser_Utils;           use Asis.Gela.Parser_Utils;

with Asis.Gela.Parser.Goto_Table;
use Asis.Gela.Parser.Goto_Table;
with Asis.Gela.Parser.Tokens;
use Asis.Gela.Parser.Tokens;
with Asis.Gela.Parser.Shift_Reduce;
use Asis.Gela.Parser.Shift_Reduce;

package body Asis.Gela.Parser is 
   package Parser_Goto renames Parser.Goto_Table;
   package Parser_Tokens renames Parser.Tokens;
   package Parser_Shift_Reduce  renames Parser.Shift_Reduce;

   package text_io renames Ada.Text_IO;

   procedure Run
     (The_Context : in     Asis.Context;
      Input       : in     Source_Buffers.Source_Buffer'Class;
      Encoding    : in     Encodings.Encoding;
      Decoder     : in     Decoders.Decoder'Class;
      Line_List   : in out Lines.Vector;
      Result      :    out Asis.Element)
   is

      Chooser : aliased Classificators.Classificator'Class :=
        Classificators_Create (Encoding, Decoder);
      Scanner : Scanners.Scanner (Chooser'Access);

      use Ada.Characters.Handling;

      Last_Compilation   : Asis.Element;
      Get_Current_Line   : Positive := 1;
      Get_Current_Column : Positive := 1;
      Line               : Lines.Line;
      Has_Comment        : Boolean := False;
      Comment            : Source_Buffers.Cursor;
      Token_Stack        : array (1 .. 300) of aliased Token_Node;
      Last_Access_Kind   : Asis.Access_Type_Kinds;
      Last_Overriding_Kind : Asis.Overriding_Indicator_Kinds;
      Last_Token         : Tokens.Token := Tokens.Token'First;

      package Modes is
         package yy is
            value_stack  : array (1 .. 300) of Mode_Kinds;
         end yy;
      end Modes;

      function Get_Token_Value return Wide_String is
         From    : Source_Buffers.Cursor;
         To      : Source_Buffers.Cursor;
         Line    : Wide_String (1 .. 2048);
         Last    : Natural;
      begin
         Scanners.Token_Span (Scanner, From, To);
         Decoders.Decode (Decoder, From, To, Line, Last);
         return Line (1 .. Last);
      end Get_Token_Value;

      function Raw_Value return Gela_String is
         From    : Source_Buffers.Cursor;
         To      : Source_Buffers.Cursor;
      begin
         Scanners.Token_Span (Scanner, From, To);
         return (From, To);
      end Raw_Value;

      function To_String (Raw : Gela_String) return Wide_String is
         Line    : Wide_String (1 .. 2048);
         Last    : Natural;
      begin
         Decoders.Decode (Decoder, Raw.From, Raw.To, Line, Last);
         return Line (1 .. Last);
      end To_String;

      function YYLex return Tokens.Token is
         use type Source_Buffers.Cursor;
         Token : Tokens.Token;
         From  : Source_Buffers.Cursor;
         To    : Source_Buffers.Cursor;
      begin
         Scanners.Next_Token (Scanner, Token);
--text_io.put_Line (Token'Img & To_String (Get_Token_Value));
         Scanners.Token_Span (Scanner, From, To);
         Get_Current_Column :=
           Get_Current_Column + Scanners.Token_Length (Scanner);

         while Token = New_Line_Token or
           Token = Separator_Token or
           Token = Comment_Token
         loop
            if Token = New_Line_Token then
               if Has_Comment then
                  Has_Comment := False;
                  Line.Comment := Comment;
               else
                  Line.Comment := From;
               end if;

               Line.To := From;
               Lines.Vectors.Add (Line_List, Line);

               Line.From := To;
               Get_Current_Line := Get_Current_Line + 1;
               Get_Current_Column := 1;
            elsif Token = Comment_Token then
               Has_Comment := True;
               Comment := From;
            end if;

            Scanners.Next_Token (Scanner, Token);
--text_io.put_Line (Token'Img & To_String (Get_Token_Value));
            Scanners.Token_Span (Scanner, From, To);
            Get_Current_Column :=
              Get_Current_Column + Scanners.Token_Length (Scanner);
         end loop;

         if Token = Identifier_Token then
            Scanners.Enter (Scanner, Scanner_Tables.Allow_Keyword);
         elsif Token = Apostrophe_Token then
            Scanners.Enter (Scanner, Scanner_Tables.Default);
         elsif Token = Interface_Token and Last_Token = Pragma_Token then
            Token := Identifier_Token;
         else
            Scanners.Enter (Scanner, Scanner_Tables.Allow_Char);
         end if;

         Last_Token := Token;
         return Token;
      end YYLex;

      pragma Inline (YYLex);

      procedure YYError (Text : String) is
         Where : constant Text_Position :=
           (Get_Current_Line, Get_Current_Column - 1);
      begin
         Report_Error
           (The_Context.all, Asis.Nil_Compilation_Unit,
            Where, "Syntax Error", Error);
      end YYError;

procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      Parser.Goto_Table;
    package yy_shift_reduce_tables renames
      Parser.Shift_Reduce;
    package yy_tokens              renames
      Parser.Tokens;

   use yy_tokens, yy_goto_tables, yy_shift_reduce_tables;

   procedure yyerrok;
   procedure yyclearin;


   package yy is

       -- the size of the value and state stacks
       stack_size : constant Natural := 300;

       -- subtype rule         is natural;
       subtype parse_state  is natural;
       -- subtype nonterminal  is integer;

       -- encryption constants
       default           : constant := -1;
       first_shift_entry : constant :=  0;
       accept_code       : constant := -3001;
       error_code        : constant := -3000;

       -- stack data used by the parser
       tos                : natural := 0;
       value_stack        : array(0..stack_size) of yy_tokens.yystype;
       state_stack        : array(0..stack_size) of parse_state;

       -- current input symbol and action the parser is on
       action             : integer;
       rule_id            : rule;
       input_symbol       : yy_tokens.token;


       -- error recovery flag
       error_flag : natural := 0;
          -- indicates  3 - (number of valid shifts after an error occurs)

       look_ahead : boolean := true;
       index      : integer;

       -- Is Debugging option on or off
        DEBUG : constant boolean := FALSE;

    end yy;


    function goto_state
      (state : yy.parse_state;
       sym   : nonterminal) return yy.parse_state;

    function parse_action
      (state : yy.parse_state;
       t     : yy_tokens.token) return integer;

    pragma inline(goto_state, parse_action);


    function goto_state(state : yy.parse_state;
                        sym   : nonterminal) return yy.parse_state is
        index : integer;
    begin
        index := goto_offset(state);
        while  integer(goto_matrix(index).nonterm) /= sym loop
            index := index + 1;
        end loop;
        return integer(goto_matrix(index).newstate);
    end goto_state;


    function parse_action(state : yy.parse_state;
                          t     : yy_tokens.token) return integer is
        index      : integer;
        tok_pos    : integer;
        default    : constant integer := -1;
    begin
        tok_pos := yy_tokens.token'pos(t);
        index   := shift_reduce_offset(state);
        while integer(shift_reduce_matrix(index).t) /= tok_pos and then
              integer(shift_reduce_matrix(index).t) /= default
        loop
            index := index + 1;
        end loop;
        return integer(shift_reduce_matrix(index).act);
    end parse_action;

-- error recovery stuff

    procedure handle_error is
      temp_action : integer;
    begin

      if yy.error_flag = 3 then -- no shift yet, clobber input.
      if yy.debug then
          text_io.put_line("Ayacc.YYParse: Error Recovery Clobbers " &
                   yy_tokens.token'image(yy.input_symbol));
      end if;
        if yy.input_symbol = yy_tokens.end_of_input then  -- don't discard,
        if yy.debug then
            text_io.put_line("Ayacc.YYParse: Can't discard END_OF_INPUT, quiting...");
        end if;
        raise yy_tokens.syntax_error;
        end if;

            yy.look_ahead := true;   -- get next token
        return;                  -- and try again...
    end if;

    if yy.error_flag = 0 then -- brand new error
        yyerror("Syntax Error");
    end if;

    yy.error_flag := 3;

    -- find state on stack where error is a valid shift --

    if yy.debug then
        text_io.put_line("Ayacc.YYParse: Looking for state with error as valid shift");
    end if;

    loop
        if yy.debug then
          text_io.put_line("Ayacc.YYParse: Examining State " &
               yy.parse_state'image(yy.state_stack(yy.tos)));
        end if;
        temp_action := parse_action(yy.state_stack(yy.tos), error);

            if temp_action >= yy.first_shift_entry then
                if yy.tos = yy.stack_size then
                    text_io.put_line(" Stack size exceeded on state_stack");
                    raise yy_Tokens.syntax_error;
                end if;
                yy.tos := yy.tos + 1;
                yy.state_stack(yy.tos) := temp_action;
                exit;
            end if;

        Decrement_Stack_Pointer :
        begin
          yy.tos := yy.tos - 1;
        exception
          when Constraint_Error =>
            yy.tos := 0;
        end Decrement_Stack_Pointer;

        if yy.tos = 0 then
          if yy.debug then
            text_io.put_line("Ayacc.YYParse: Error recovery popped entire stack, aborting...");
          end if;
          raise yy_tokens.syntax_error;
        end if;
    end loop;

    if yy.debug then
        text_io.put_line("Ayacc.YYParse: Shifted error token in state " &
              yy.parse_state'image(yy.state_stack(yy.tos)));
    end if;

    end handle_error;

   -- print debugging information for a shift operation
   procedure shift_debug(state_id: yy.parse_state; lexeme: yy_tokens.token) is
   begin
       text_io.put_line("Ayacc.YYParse: Shift "& yy.parse_state'image(state_id)&" on input symbol "&
               yy_tokens.token'image(lexeme) );
   end;

   -- print debugging information for a reduce operation
   procedure reduce_debug(rule_id: rule; state_id: yy.parse_state) is
   begin
       text_io.put_line("Ayacc.YYParse: Reduce by rule "&rule'image(rule_id)&" goto state "&
               yy.parse_state'image(state_id));
   end;

   -- make the parser believe that 3 valid shifts have occured.
   -- used for error recovery.
   procedure yyerrok is
   begin
       yy.error_flag := 0;
   end yyerrok;

   -- called to clear input symbol that caused an error.
   procedure yyclearin is
   begin
       -- yy.input_symbol := yylex;
       yy.look_ahead := true;
   end yyclearin;


begin
    -- initialize by pushing state 0 and getting the first input symbol
    yy.state_stack(yy.tos) := 0;


    loop

        yy.index := shift_reduce_offset(yy.state_stack(yy.tos));
        if integer(shift_reduce_matrix(yy.index).t) = yy.default then
            yy.action := integer(shift_reduce_matrix(yy.index).act);
        else
            if yy.look_ahead then
                yy.look_ahead   := false;

                yy.input_symbol := yylex;
            end if;
            yy.action :=
             parse_action(yy.state_stack(yy.tos), yy.input_symbol);
        end if;


        if yy.action >= yy.first_shift_entry then  -- SHIFT

            if yy.debug then
                shift_debug(yy.action, yy.input_symbol);
            end if;

            -- Enter new state
            if yy.tos = yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.tos := yy.tos + 1;
            yy.state_stack(yy.tos) := yy.action;
              yy.value_stack(yy.tos) := yylval;

        if yy.error_flag > 0 then  -- indicate a valid shift
            yy.error_flag := yy.error_flag - 1;
        end if;

            -- Advance lookahead
            yy.look_ahead := true;

        elsif yy.action = yy.error_code then       -- ERROR

            handle_error;

        elsif yy.action = yy.accept_code then
            if yy.debug then
                text_io.put_line("Ayacc.YYParse: Accepting Grammar...");
            end if;
            exit;

        else -- Reduce Action

            -- Convert action into a rule
            yy.rule_id  := -1 * yy.action;

            -- Execute User Action
            -- user_action(yy.rule_id);


                case yy.rule_id is

when  1 =>
--#line  118

   declare
      Node :  constant Identifier_Ptr := New_Identifier_Node (The_Context);
      Value : constant Wide_String := Get_Token_Value;
   begin
      Set_Name_Image (Node.all, Value);
      Set_Start_Position (Node.all,
        (Get_Current_Line, Get_Current_Column - Value'Length));
      Set_End_Position (Node.all,
        (Get_Current_Line, Get_Current_Column - 1));
      
yyval := YYSTYPE (Node);
   end;
   

when  2 =>
--#line  133

   declare
      Node :  constant Integer_Literal_Ptr := New_Integer_Literal_Node (The_Context);
      Value : constant Wide_String := Get_Token_Value;
   begin
      Set_Value_Image (Node.all, Value);
      Set_Start_Position (Node.all,
        (Get_Current_Line, Get_Current_Column - Value'Length));
      Set_End_Position (Node.all,
        (Get_Current_Line, Get_Current_Column - 1));
      
yyval := YYSTYPE (Node);
   end;
   

when  3 =>
--#line  148

   declare
      Node :  constant Real_Literal_Ptr := New_Real_Literal_Node (The_Context);
      Value : constant Wide_String := Get_Token_Value;
   begin
      Set_Value_Image (Node.all, Value);
      Set_Start_Position (Node.all,
        (Get_Current_Line, Get_Current_Column - Value'Length));
      Set_End_Position (Node.all,
        (Get_Current_Line, Get_Current_Column - 1));
      
yyval := YYSTYPE (Node);
   end;
   

when  4 =>
--#line  163

   declare
      Node :  constant Character_Literal_Ptr := New_Character_Literal_Node (The_Context);
      Value : constant Wide_String := Get_Token_Value;
   begin
      Set_Name_Image (Node.all, Value);
      Set_Start_Position (Node.all,
        (Get_Current_Line, Get_Current_Column - Value'Length));
      Set_End_Position (Node.all,
        (Get_Current_Line, Get_Current_Column - 1));
      
yyval := YYSTYPE (Node);
   end;
   

when  5 =>
--#line  178

   declare
      Node :  constant String_Literal_Ptr := New_String_Literal_Node (The_Context);
      Value : constant Wide_String := Get_Token_Value;
   begin
      Set_Value_Image (Node.all, Value);
      Set_Start_Position (Node.all,
        (Get_Current_Line, Get_Current_Column - Value'Length));
      Set_End_Position (Node.all,
        (Get_Current_Line, Get_Current_Column - 1));
      
yyval := YYSTYPE (Node);
   end;
   

when  6 =>
--#line  193

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  7 =>
--#line  207

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  8 =>
--#line  221

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  9 =>
--#line  235

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  10 =>
--#line  249

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  11 =>
--#line  263

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  12 =>
--#line  277

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  13 =>
--#line  291

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  14 =>
--#line  305

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  15 =>
--#line  319

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  16 =>
--#line  333

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  17 =>
--#line  347

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  18 =>
--#line  361

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  19 =>
--#line  375

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  20 =>
--#line  389

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  21 =>
--#line  403

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  22 =>
--#line  417

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  23 =>
--#line  431

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  24 =>
--#line  445

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  25 =>
--#line  459

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  26 =>
--#line  473

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  27 =>
--#line  487

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  28 =>
--#line  501

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  29 =>
--#line  515

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  30 =>
--#line  529

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  31 =>
--#line  543

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 1);
      
yyval := YYSTYPE (Node);
   end;
   

when  32 =>
--#line  557

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 5);
      
yyval := YYSTYPE (Node);
   end;
   

when  33 =>
--#line  571

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 3);
      
yyval := YYSTYPE (Node);
   end;
   

when  34 =>
--#line  585

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 8);
      
yyval := YYSTYPE (Node);
   end;
   

when  35 =>
--#line  599

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 6);
      
yyval := YYSTYPE (Node);
   end;
   

when  36 =>
--#line  613

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 6);
      
yyval := YYSTYPE (Node);
   end;
   

when  37 =>
--#line  627

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 7);
      
yyval := YYSTYPE (Node);
   end;
   

when  38 =>
--#line  641

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 3);
      
yyval := YYSTYPE (Node);
   end;
   

when  39 =>
--#line  655

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 3);
      
yyval := YYSTYPE (Node);
   end;
   

when  40 =>
--#line  669

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 5);
      
yyval := YYSTYPE (Node);
   end;
   

when  41 =>
--#line  683

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  42 =>
--#line  697

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 5);
      
yyval := YYSTYPE (Node);
   end;
   

when  43 =>
--#line  711

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 4);
      
yyval := YYSTYPE (Node);
   end;
   

when  44 =>
--#line  725

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 4);
      
yyval := YYSTYPE (Node);
   end;
   

when  45 =>
--#line  739

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 8);
      
yyval := YYSTYPE (Node);
   end;
   

when  46 =>
--#line  753

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 7);
      
yyval := YYSTYPE (Node);
   end;
   

when  47 =>
--#line  767

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 5);
      
yyval := YYSTYPE (Node);
   end;
   

when  48 =>
--#line  781

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 5);
      
yyval := YYSTYPE (Node);
   end;
   

when  49 =>
--#line  795

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 6);
      
yyval := YYSTYPE (Node);
   end;
   

when  50 =>
--#line  809

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  51 =>
--#line  823

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 4);
      
yyval := YYSTYPE (Node);
   end;
   

when  52 =>
--#line  837

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 5);
      
yyval := YYSTYPE (Node);
   end;
   

when  53 =>
--#line  851

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 3);
      
yyval := YYSTYPE (Node);
   end;
   

when  54 =>
--#line  865

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 5);
      
yyval := YYSTYPE (Node);
   end;
   

when  55 =>
--#line  879

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 9);
      
yyval := YYSTYPE (Node);
   end;
   

when  56 =>
--#line  893

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 4);
      
yyval := YYSTYPE (Node);
   end;
   

when  57 =>
--#line  907

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 3);
      
yyval := YYSTYPE (Node);
   end;
   

when  58 =>
--#line  921

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 8);
      
yyval := YYSTYPE (Node);
   end;
   

when  59 =>
--#line  935

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 7);
      
yyval := YYSTYPE (Node);
   end;
   

when  60 =>
--#line  949

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 4);
      
yyval := YYSTYPE (Node);
   end;
   

when  61 =>
--#line  963

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  62 =>
--#line  977

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  63 =>
--#line  991

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 9);
      
yyval := YYSTYPE (Node);
   end;
   

when  64 =>
--#line  1005

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  65 =>
--#line  1019

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 7);
      
yyval := YYSTYPE (Node);
   end;
   

when  66 =>
--#line  1033

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 4);
      
yyval := YYSTYPE (Node);
   end;
   

when  67 =>
--#line  1047

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 3);
      
yyval := YYSTYPE (Node);
   end;
   

when  68 =>
--#line  1061

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 3);
      
yyval := YYSTYPE (Node);
   end;
   

when  69 =>
--#line  1075

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 3);
      
yyval := YYSTYPE (Node);
   end;
   

when  70 =>
--#line  1089

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 4);
      
yyval := YYSTYPE (Node);
   end;
   

when  71 =>
--#line  1103

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  72 =>
--#line  1117

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 2);
      
yyval := YYSTYPE (Node);
   end;
   

when  73 =>
--#line  1131

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 6);
      
yyval := YYSTYPE (Node);
   end;
   

when  74 =>
--#line  1145

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 3);
      
yyval := YYSTYPE (Node);
   end;
   

when  75 =>
--#line  1159

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 10);
      
yyval := YYSTYPE (Node);
   end;
   

when  76 =>
--#line  1173

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 7);
      
yyval := YYSTYPE (Node);
   end;
   

when  77 =>
--#line  1187

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 6);
      
yyval := YYSTYPE (Node);
   end;
   

when  78 =>
--#line  1201

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 7);
      
yyval := YYSTYPE (Node);
   end;
   

when  79 =>
--#line  1215

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 9);
      
yyval := YYSTYPE (Node);
   end;
   

when  80 =>
--#line  1229

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 9);
      
yyval := YYSTYPE (Node);
   end;
   

when  81 =>
--#line  1243

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 5);
      
yyval := YYSTYPE (Node);
   end;
   

when  82 =>
--#line  1257

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 5);
      
yyval := YYSTYPE (Node);
   end;
   

when  83 =>
--#line  1271

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 6);
      
yyval := YYSTYPE (Node);
   end;
   

when  84 =>
--#line  1285

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 3);
      
yyval := YYSTYPE (Node);
   end;
   

when  85 =>
--#line  1299

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 7);
      
yyval := YYSTYPE (Node);
   end;
   

when  86 =>
--#line  1313

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 7);
      
yyval := YYSTYPE (Node);
   end;
   

when  87 =>
--#line  1327

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 6);
      
yyval := YYSTYPE (Node);
   end;
   

when  88 =>
--#line  1341

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 7);
      
yyval := YYSTYPE (Node);
   end;
   

when  89 =>
--#line  1355

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 6);
      
yyval := YYSTYPE (Node);
   end;
   

when  90 =>
--#line  1369

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 8);
      
yyval := YYSTYPE (Node);
   end;
   

when  91 =>
--#line  1383

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 7);
      
yyval := YYSTYPE (Node);
   end;
   

when  92 =>
--#line  1397

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 12);
      
yyval := YYSTYPE (Node);
   end;
   

when  93 =>
--#line  1411

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 6);
      
yyval := YYSTYPE (Node);
   end;
   

when  94 =>
--#line  1425

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 4);
      
yyval := YYSTYPE (Node);
   end;
   

when  95 =>
--#line  1439

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 9);
      
yyval := YYSTYPE (Node);
   end;
   

when  96 =>
--#line  1453

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 4);
      
yyval := YYSTYPE (Node);
   end;
   

when  97 =>
--#line  1467

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 4);
      
yyval := YYSTYPE (Node);
   end;
   

when  98 =>
--#line  1481

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 5);
      
yyval := YYSTYPE (Node);
   end;
   

when  99 =>
--#line  1495

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 3);
      
yyval := YYSTYPE (Node);
   end;
   

when  100 =>
--#line  1509

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 4);
      
yyval := YYSTYPE (Node);
   end;
   

when  101 =>
--#line  1523

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 5);
      
yyval := YYSTYPE (Node);
   end;
   

when  102 =>
--#line  1537

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 4);
      
yyval := YYSTYPE (Node);
   end;
   

when  103 =>
--#line  1551

   declare
      Node :  constant Token_Ptr := Token_Stack (yy.tos)'Unchecked_Access;
   begin
      Init_Token (Element => Node.all,
                  Line    => Get_Current_Line,
                  Column  => Get_Current_Column,
                  Image   => Raw_Value,
                  Length  => 3);
      
yyval := YYSTYPE (Node);
   end;
   

when  104 =>
--#line  1566

   declare
      New_Node : constant Pragma_Ptr :=
        New_Pragma_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Pragma_Name_Image (New_Node.all, Name_Image (Identifier_Ptr (
yy.value_stack(yy.tos-2)).all));
      Set_Pragma_Argument_Associations (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  105 =>
--#line  1579

   declare
      New_Node : constant Pragma_Ptr :=
        New_Pragma_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Pragma_Name_Image (New_Node.all, Name_Image (Identifier_Ptr (
yy.value_stack(yy.tos-1)).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  106 =>
--#line  1594

   declare
      New_Node : constant Pragma_Argument_Association_Ptr :=
        New_Pragma_Argument_Association_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Formal_Parameter (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Actual_Parameter (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  107 =>
--#line  1607

   declare
      New_Node : constant Pragma_Argument_Association_Ptr :=
        New_Pragma_Argument_Association_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Actual_Parameter (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  108 =>
--#line  1622
 
yyval := 
yy.value_stack(yy.tos);

when  109 =>
--#line  1624
 
yyval := 
yy.value_stack(yy.tos);

when  110 =>
--#line  1626
 
yyval := 
yy.value_stack(yy.tos);

when  111 =>
--#line  1628
 
yyval := 
yy.value_stack(yy.tos);

when  112 =>
--#line  1630
 
yyval := 
yy.value_stack(yy.tos);

when  113 =>
--#line  1632
 
yyval := 
yy.value_stack(yy.tos);

when  114 =>
--#line  1634
 
yyval := 
yy.value_stack(yy.tos);

when  115 =>
--#line  1636
 
yyval := 
yy.value_stack(yy.tos);

when  116 =>
--#line  1638
 
yyval := 
yy.value_stack(yy.tos);

when  117 =>
--#line  1640
 
yyval := 
yy.value_stack(yy.tos);

when  118 =>
--#line  1642
 
yyval := 
yy.value_stack(yy.tos);

when  119 =>
--#line  1644
 
yyval := 
yy.value_stack(yy.tos);

when  120 =>
--#line  1649

   declare
      New_Node : constant Defining_Identifier_Ptr :=
        New_Defining_Identifier_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Defining_Name_Image (New_Node.all, Name_Image (Identifier_Ptr (
yy.value_stack(yy.tos)).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  121 =>
--#line  1664
 
yyval := 
yy.value_stack(yy.tos);

when  122 =>
--#line  1666
 
yyval := 
yy.value_stack(yy.tos);

when  123 =>
--#line  1668
 
yyval := 
yy.value_stack(yy.tos);

when  124 =>
--#line  1670
 
yyval := 
yy.value_stack(yy.tos);

when  125 =>
--#line  1675

   declare
      New_Node : constant Ordinary_Type_Declaration_Ptr :=
        New_Ordinary_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Discriminant_Part (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  126 =>
--#line  1689

   declare
      New_Node : constant Ordinary_Type_Declaration_Ptr :=
        New_Ordinary_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  127 =>
--#line  1702
 
yyval := 
yy.value_stack(yy.tos);

when  128 =>
--#line  1704
 
yyval := 
yy.value_stack(yy.tos);

when  129 =>
--#line  1709
 
yyval := 
yy.value_stack(yy.tos);

when  130 =>
--#line  1711
 
yyval := 
yy.value_stack(yy.tos);

when  131 =>
--#line  1713
 
yyval := 
yy.value_stack(yy.tos);

when  132 =>
--#line  1715
 
yyval := 
yy.value_stack(yy.tos);

when  133 =>
--#line  1717
 
yyval := 
yy.value_stack(yy.tos);

when  134 =>
--#line  1719
 
yyval := 
yy.value_stack(yy.tos);

when  135 =>
--#line  1721
 
yyval := 
yy.value_stack(yy.tos);

when  136 =>
--#line  1723
 
yyval := 
yy.value_stack(yy.tos);

when  137 =>
--#line  1728

   declare
      New_Node : constant Subtype_Declaration_Ptr :=
        New_Subtype_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  138 =>
--#line  1744

   declare
      New_Node : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Has_Null_Exclusion (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Subtype_Constraint (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  139 =>
--#line  1758

   declare
      New_Node : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Null_Exclusion (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  140 =>
--#line  1771

   declare
      New_Node : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Subtype_Constraint (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  141 =>
--#line  1784

   declare
      New_Node : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  142 =>
--#line  1799
 
yyval := 
yy.value_stack(yy.tos);

when  143 =>
--#line  1804
 
yyval := 
yy.value_stack(yy.tos);

when  144 =>
--#line  1806
 
yyval := 
yy.value_stack(yy.tos);

when  145 =>
--#line  1811
 
yyval := 
yy.value_stack(yy.tos);

when  146 =>
--#line  1813
 
yyval := 
yy.value_stack(yy.tos);

when  147 =>
--#line  1815
 
yyval := 
yy.value_stack(yy.tos);

when  148 =>
--#line  1820
 
yyval := 
yy.value_stack(yy.tos);

when  149 =>
--#line  1822
 
yyval := 
yy.value_stack(yy.tos);

when  150 =>
--#line  1827

   declare
      New_Node : constant Constant_Declaration_Ptr :=
        New_Constant_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-6));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  151 =>
--#line  1842

   declare
      New_Node : constant Constant_Declaration_Ptr :=
        New_Constant_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  152 =>
--#line  1856

   declare
      New_Node : constant Variable_Declaration_Ptr :=
        New_Variable_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  153 =>
--#line  1871

   declare
      New_Node : constant Variable_Declaration_Ptr :=
        New_Variable_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  154 =>
--#line  1885

   declare
      New_Node : constant Constant_Declaration_Ptr :=
        New_Constant_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  155 =>
--#line  1899

   declare
      New_Node : constant Constant_Declaration_Ptr :=
        New_Constant_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  156 =>
--#line  1912

   declare
      New_Node : constant Variable_Declaration_Ptr :=
        New_Variable_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  157 =>
--#line  1926

   declare
      New_Node : constant Variable_Declaration_Ptr :=
        New_Variable_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  158 =>
--#line  1939

   declare
      New_Node : constant Constant_Declaration_Ptr :=
        New_Constant_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-6));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  159 =>
--#line  1954

   declare
      New_Node : constant Constant_Declaration_Ptr :=
        New_Constant_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  160 =>
--#line  1968

   declare
      New_Node : constant Variable_Declaration_Ptr :=
        New_Variable_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  161 =>
--#line  1983

   declare
      New_Node : constant Variable_Declaration_Ptr :=
        New_Variable_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  162 =>
--#line  1997

   declare
      New_Node : constant Constant_Declaration_Ptr :=
        New_Constant_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  163 =>
--#line  2011

   declare
      New_Node : constant Constant_Declaration_Ptr :=
        New_Constant_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  164 =>
--#line  2024

   declare
      New_Node : constant Variable_Declaration_Ptr :=
        New_Variable_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  165 =>
--#line  2038

   declare
      New_Node : constant Variable_Declaration_Ptr :=
        New_Variable_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  166 =>
--#line  2051

   declare
      New_Node : constant Constant_Declaration_Ptr :=
        New_Constant_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-6));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  167 =>
--#line  2066

   declare
      New_Node : constant Constant_Declaration_Ptr :=
        New_Constant_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  168 =>
--#line  2080

   declare
      New_Node : constant Variable_Declaration_Ptr :=
        New_Variable_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  169 =>
--#line  2095

   declare
      New_Node : constant Variable_Declaration_Ptr :=
        New_Variable_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  170 =>
--#line  2109

   declare
      New_Node : constant Constant_Declaration_Ptr :=
        New_Constant_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  171 =>
--#line  2123

   declare
      New_Node : constant Constant_Declaration_Ptr :=
        New_Constant_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  172 =>
--#line  2136

   declare
      New_Node : constant Variable_Declaration_Ptr :=
        New_Variable_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  173 =>
--#line  2150

   declare
      New_Node : constant Variable_Declaration_Ptr :=
        New_Variable_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  174 =>
--#line  2163
 
yyval := 
yy.value_stack(yy.tos);

when  175 =>
--#line  2165
 
yyval := 
yy.value_stack(yy.tos);

when  176 =>
--#line  2170

   declare
      Wrap1 : constant Primary_Defining_Name_Lists.List :=
        Primary_Defining_Name_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Defining_Name_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  177 =>
--#line  2180

   declare
      Wrap1 : constant Primary_Defining_Name_Lists.List :=
        Primary_Defining_Name_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Defining_Name_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos));
   end;


when  178 =>
--#line  2193

   declare
      New_Node : constant Integer_Number_Declaration_Ptr :=
        New_Integer_Number_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  179 =>
--#line  2209

   declare
      New_Node : constant Derived_Record_Extension_Ptr :=
        New_Derived_Record_Extension_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Has_Limited (New_Node.all, True);
      Set_Parent_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  180 =>
--#line  2226

   declare
      New_Node : constant Derived_Record_Extension_Ptr :=
        New_Derived_Record_Extension_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Has_Limited (New_Node.all, True);
      Set_Parent_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  181 =>
--#line  2242

   declare
      New_Node : constant Derived_Record_Extension_Ptr :=
        New_Derived_Record_Extension_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Parent_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  182 =>
--#line  2258

   declare
      New_Node : constant Derived_Record_Extension_Ptr :=
        New_Derived_Record_Extension_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Parent_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  183 =>
--#line  2273

   declare
      New_Node : constant Derived_Record_Extension_Ptr :=
        New_Derived_Record_Extension_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Has_Limited (New_Node.all, True);
      Set_Parent_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  184 =>
--#line  2288

   declare
      New_Node : constant Derived_Record_Extension_Ptr :=
        New_Derived_Record_Extension_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Has_Limited (New_Node.all, True);
      Set_Parent_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  185 =>
--#line  2302

   declare
      New_Node : constant Derived_Record_Extension_Ptr :=
        New_Derived_Record_Extension_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Parent_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  186 =>
--#line  2316

   declare
      New_Node : constant Derived_Record_Extension_Ptr :=
        New_Derived_Record_Extension_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Parent_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  187 =>
--#line  2329

   declare
      New_Node : constant Derived_Type_Ptr :=
        New_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Has_Limited (New_Node.all, True);
      Set_Parent_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  188 =>
--#line  2344

   declare
      New_Node : constant Derived_Type_Ptr :=
        New_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Parent_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  189 =>
--#line  2358

   declare
      New_Node : constant Derived_Type_Ptr :=
        New_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Has_Limited (New_Node.all, True);
      Set_Parent_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  190 =>
--#line  2371

   declare
      New_Node : constant Derived_Type_Ptr :=
        New_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Parent_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  191 =>
--#line  2386

   declare
      Wrap1 : constant Constraint_Ptr :=
        Constraint_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
   end;


when  192 =>
--#line  2399
 
yyval := 
yy.value_stack(yy.tos);

when  193 =>
--#line  2401

   declare
      New_Node : constant Simple_Expression_Range_Ptr :=
        New_Simple_Expression_Range_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Lower_Bound (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Upper_Bound (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  194 =>
--#line  2417

   declare
      Wrap1 : constant Enumeration_Type_Ptr :=
        New_Enumeration_Type_Node (The_Context);
      Wrap2 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Primary_Declaration_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Enumeration_Literal_Declarations (Wrap1.all, T (Wrap2));
   end;


when  195 =>
--#line  2432

   declare
      Wrap1 : constant Enumeration_Type_Ptr :=
        New_Enumeration_Type_Node (The_Context);
      Wrap2 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Primary_Declaration_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Enumeration_Literal_Declarations (Wrap1.all, T (Wrap2));
   end;


when  196 =>
--#line  2450

   declare
      Wrap1 : constant Enumeration_Literal_Specification_Ptr :=
        New_Enumeration_Literal_Specification_Node (The_Context);
      Wrap2 : constant Defining_Enumeration_Literal_Ptr :=
        New_Defining_Enumeration_Literal_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Defining_Name_Image (Wrap2.all, Defining_Name_Image (Defining_Identifier_Ptr (
yy.value_stack(yy.tos)).all));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Name (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  197 =>
--#line  2467

   declare
      New_Node : constant Enumeration_Literal_Specification_Ptr :=
        New_Enumeration_Literal_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  198 =>
--#line  2482

   declare
      New_Node : constant Defining_Character_Literal_Ptr :=
        New_Defining_Character_Literal_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Defining_Name_Image (New_Node.all, Name_Image (Character_Literal_Ptr (
yy.value_stack(yy.tos)).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  199 =>
--#line  2497
 
yyval := 
yy.value_stack(yy.tos);

when  200 =>
--#line  2499
 
yyval := 
yy.value_stack(yy.tos);

when  201 =>
--#line  2504

   declare
      New_Node : constant Signed_Integer_Type_Ptr :=
        New_Signed_Integer_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Integer_Constraint (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  202 =>
--#line  2519

   declare
      New_Node : constant Modular_Type_Ptr :=
        New_Modular_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Mod_Static_Expression (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  203 =>
--#line  2534
 
yyval := 
yy.value_stack(yy.tos);

when  204 =>
--#line  2536
 
yyval := 
yy.value_stack(yy.tos);

when  205 =>
--#line  2541

   declare
      New_Node : constant Floating_Point_Ptr :=
        New_Floating_Point_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Digits_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Real_Range_Constraint (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  206 =>
--#line  2554

   declare
      New_Node : constant Floating_Point_Ptr :=
        New_Floating_Point_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Digits_Expression (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  207 =>
--#line  2569

   declare
      New_Node : constant Simple_Expression_Range_Ptr :=
        New_Simple_Expression_Range_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Lower_Bound (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Upper_Bound (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  208 =>
--#line  2585
 
yyval := 
yy.value_stack(yy.tos);

when  209 =>
--#line  2587
 
yyval := 
yy.value_stack(yy.tos);

when  210 =>
--#line  2592

   declare
      New_Node : constant Ordinary_Fixed_Point_Ptr :=
        New_Ordinary_Fixed_Point_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Delta_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Real_Range_Constraint (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  211 =>
--#line  2608

   declare
      New_Node : constant Decimal_Fixed_Point_Ptr :=
        New_Decimal_Fixed_Point_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Delta_Expression (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Digits_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Real_Range_Constraint (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  212 =>
--#line  2622

   declare
      New_Node : constant Decimal_Fixed_Point_Ptr :=
        New_Decimal_Fixed_Point_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Delta_Expression (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Digits_Expression (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  213 =>
--#line  2638

   declare
      New_Node : constant Digits_Constraint_Ptr :=
        New_Digits_Constraint_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Digits_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Real_Range_Constraint (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  214 =>
--#line  2651

   declare
      New_Node : constant Digits_Constraint_Ptr :=
        New_Digits_Constraint_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Digits_Expression (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  215 =>
--#line  2666
 
yyval := 
yy.value_stack(yy.tos);

when  216 =>
--#line  2668
 
yyval := 
yy.value_stack(yy.tos);

when  217 =>
--#line  2673

   declare
      Wrap1 : constant Unconstrained_Array_Ptr :=
        New_Unconstrained_Array_Node (The_Context);
      Wrap2 : constant Primary_Identifier_Lists.List :=
        Primary_Identifier_Lists.List (
yy.value_stack(yy.tos-3));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Primary_Identifier_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Array_Component_Definition (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Index_Subtype_Definitions (Wrap1.all, T (Wrap2));
   end;


when  218 =>
--#line  2689

   declare
      Wrap1 : constant Unconstrained_Array_Ptr :=
        New_Unconstrained_Array_Node (The_Context);
      Wrap2 : constant Primary_Identifier_Lists.List :=
        Primary_Identifier_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Primary_Identifier_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Array_Component_Definition (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Index_Subtype_Definitions (Wrap1.all, T (Wrap2));
   end;


when  219 =>
--#line  2708
 
yyval := 
yy.value_stack(yy.tos-2);

when  220 =>
--#line  2713

   declare
      Wrap1 : constant Constrained_Array_Ptr :=
        New_Constrained_Array_Node (The_Context);
      Wrap2 : constant Primary_Definition_Lists.List :=
        Primary_Definition_Lists.List (
yy.value_stack(yy.tos-3));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Primary_Definition_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Array_Component_Definition (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Discrete_Subtype_Definitions (Wrap1.all, T (Wrap2));
   end;


when  221 =>
--#line  2729

   declare
      Wrap1 : constant Constrained_Array_Ptr :=
        New_Constrained_Array_Node (The_Context);
      Wrap2 : constant Primary_Definition_Lists.List :=
        Primary_Definition_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Primary_Definition_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Array_Component_Definition (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Discrete_Subtype_Definitions (Wrap1.all, T (Wrap2));
   end;


when  222 =>
--#line  2748

   declare
      New_Node : constant S_Discrete_Subtype_Indication_Ptr :=
        New_S_Discrete_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  223 =>
--#line  2760

   declare
      New_Node : constant S_Discrete_Range_Attribute_Reference_Ptr :=
        New_S_Discrete_Range_Attribute_Reference_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Range_Attribute (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  224 =>
--#line  2772

   declare
      New_Node : constant S_Discrete_Simple_Expression_Range_Ptr :=
        New_S_Discrete_Simple_Expression_Range_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Lower_Bound (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Upper_Bound (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  225 =>
--#line  2788

   declare
      New_Node : constant Component_Definition_Ptr :=
        New_Component_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Component_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  226 =>
--#line  2801

   declare
      New_Node : constant Component_Definition_Ptr :=
        New_Component_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Component_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  227 =>
--#line  2813

   declare
      New_Node : constant Component_Definition_Ptr :=
        New_Component_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Trait_Kind (New_Node.all, An_Aliased_Trait);
      Set_Component_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  228 =>
--#line  2826

   declare
      New_Node : constant Component_Definition_Ptr :=
        New_Component_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Component_Subtype_Indication (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  229 =>
--#line  2841
 
yyval := 
yy.value_stack(yy.tos-2);

when  230 =>
--#line  2843
 
yyval := 
yy.value_stack(yy.tos-1);

when  231 =>
--#line  2848

   declare
      New_Node : constant Discrete_Subtype_Indication_Ptr :=
        New_Discrete_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Subtype_Constraint (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  232 =>
--#line  2861

   declare
      New_Node : constant Discrete_Range_Attribute_Reference_Ptr :=
        New_Discrete_Range_Attribute_Reference_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Range_Attribute (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  233 =>
--#line  2873

   declare
      New_Node : constant Discrete_Simple_Expression_Range_Ptr :=
        New_Discrete_Simple_Expression_Range_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Lower_Bound (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Upper_Bound (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  234 =>
--#line  2889
 
yyval := 
yy.value_stack(yy.tos);

when  235 =>
--#line  2891
 
yyval := 
yy.value_stack(yy.tos);

when  236 =>
--#line  2896

   declare
      New_Node : constant Unknown_Discriminant_Part_Ptr :=
        New_Unknown_Discriminant_Part_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  237 =>
--#line  2910

   declare
      Wrap1 : constant Known_Discriminant_Part_Ptr :=
        New_Known_Discriminant_Part_Node (The_Context);
      Wrap2 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Primary_Declaration_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Discriminants (Wrap1.all, T (Wrap2));
   end;


when  238 =>
--#line  2925

   declare
      Wrap1 : constant Known_Discriminant_Part_Ptr :=
        New_Known_Discriminant_Part_Node (The_Context);
      Wrap2 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Primary_Declaration_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Discriminants (Wrap1.all, T (Wrap2));
   end;


when  239 =>
--#line  2943

   declare
      Wrap1 : constant Discriminant_Specification_Ptr :=
        New_Discriminant_Specification_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Has_Null_Exclusion (Wrap1.all, True);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Initialization_Expression (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  240 =>
--#line  2963

   declare
      Wrap1 : constant Discriminant_Specification_Ptr :=
        New_Discriminant_Specification_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Null_Exclusion (Wrap1.all, True);
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  241 =>
--#line  2983

   declare
      Wrap1 : constant Discriminant_Specification_Ptr :=
        New_Discriminant_Specification_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Initialization_Expression (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  242 =>
--#line  3002

   declare
      Wrap1 : constant Discriminant_Specification_Ptr :=
        New_Discriminant_Specification_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  243 =>
--#line  3021

   declare
      New_Node : constant Discriminant_Specification_Ptr :=
        New_Discriminant_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Trait_Kind (New_Node.all, An_Access_Definition_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  244 =>
--#line  3036

   declare
      New_Node : constant Discriminant_Specification_Ptr :=
        New_Discriminant_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, An_Access_Definition_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  245 =>
--#line  3053
 
yyval := 
yy.value_stack(yy.tos);

when  246 =>
--#line  3058
 
yyval := 
yy.value_stack(yy.tos-2);

when  247 =>
--#line  3060
 
yyval := 
yy.value_stack(yy.tos-1);

when  248 =>
--#line  3065
 
yyval := 
yy.value_stack(yy.tos);

when  249 =>
--#line  3067
 
yyval := 
yy.value_stack(yy.tos);

when  250 =>
--#line  3072

   declare
      New_Node : constant Tagged_Record_Type_Ptr :=
        New_Tagged_Record_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Has_Tagged (New_Node.all, True);
      Set_Trait_Kind (New_Node.all, A_Limited_Trait);
      Set_Has_Limited (New_Node.all, True);
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  251 =>
--#line  3089

   declare
      New_Node : constant Tagged_Record_Type_Ptr :=
        New_Tagged_Record_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Has_Tagged (New_Node.all, True);
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  252 =>
--#line  3104

   declare
      New_Node : constant Tagged_Record_Type_Ptr :=
        New_Tagged_Record_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Has_Tagged (New_Node.all, True);
      Set_Trait_Kind (New_Node.all, A_Limited_Trait);
      Set_Has_Limited (New_Node.all, True);
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  253 =>
--#line  3119

   declare
      New_Node : constant Tagged_Record_Type_Ptr :=
        New_Tagged_Record_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Tagged (New_Node.all, True);
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  254 =>
--#line  3132

   declare
      New_Node : constant Record_Type_Ptr :=
        New_Record_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Trait_Kind (New_Node.all, A_Limited_Trait);
      Set_Has_Limited (New_Node.all, True);
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  255 =>
--#line  3146

   declare
      New_Node : constant Record_Type_Ptr :=
        New_Record_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Record_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  256 =>
--#line  3161

   declare
      New_Node : constant Record_Definition_Ptr :=
        New_Record_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Record_Components (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  257 =>
--#line  3173

   declare
      New_Node : constant Null_Record_Definition_Ptr :=
        New_Null_Record_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  258 =>
--#line  3187

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Declaration_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos));
   end;


when  259 =>
--#line  3197

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
   end;


when  260 =>
--#line  3206

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Declaration_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos));
   end;


when  261 =>
--#line  3216

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
   end;


when  262 =>
--#line  3225

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
      Wrap2 : constant Null_Component_Ptr :=
        New_Null_Component_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Pragmas (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Declaration_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  263 =>
--#line  3240

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
      Wrap2 : constant Null_Component_Ptr :=
        New_Null_Component_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Declaration_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  264 =>
--#line  3257
 
yyval := 
yy.value_stack(yy.tos);

when  265 =>
--#line  3259
 
yyval := 
yy.value_stack(yy.tos);

when  266 =>
--#line  3264

   declare
      New_Node : constant Component_Declaration_Ptr :=
        New_Component_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  267 =>
--#line  3278

   declare
      New_Node : constant Component_Declaration_Ptr :=
        New_Component_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  268 =>
--#line  3294

   declare
      Wrap1 : constant Variant_Part_Ptr :=
        New_Variant_Part_Node (The_Context);
      Wrap2 : constant Primary_Variant_Lists.List :=
        Primary_Variant_Lists.List (
yy.value_stack(yy.tos-3));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Discriminant_Direct_Name (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Pragmas (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Primary_Variant_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Variants (Wrap1.all, T (Wrap2));
   end;


when  269 =>
--#line  3311

   declare
      Wrap1 : constant Variant_Part_Ptr :=
        New_Variant_Part_Node (The_Context);
      Wrap2 : constant Primary_Variant_Lists.List :=
        Primary_Variant_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Discriminant_Direct_Name (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Set_Pragmas (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Primary_Variant_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Variants (Wrap1.all, T (Wrap2));
   end;


when  270 =>
--#line  3328

   declare
      Wrap1 : constant Variant_Part_Ptr :=
        New_Variant_Part_Node (The_Context);
      Wrap2 : constant Primary_Variant_Lists.List :=
        Primary_Variant_Lists.List (
yy.value_stack(yy.tos-3));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Discriminant_Direct_Name (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Primary_Variant_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Variants (Wrap1.all, T (Wrap2));
   end;


when  271 =>
--#line  3344

   declare
      Wrap1 : constant Variant_Part_Ptr :=
        New_Variant_Part_Node (The_Context);
      Wrap2 : constant Primary_Variant_Lists.List :=
        Primary_Variant_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Discriminant_Direct_Name (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Primary_Variant_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Variants (Wrap1.all, T (Wrap2));
   end;


when  272 =>
--#line  3363

   declare
      New_Node : constant Variant_Ptr :=
        New_Variant_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Variant_Choices (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Record_Components (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  273 =>
--#line  3379

   declare
      Wrap1 : constant Primary_Choise_Lists.List :=
        Primary_Choise_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Choise_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  274 =>
--#line  3389

   declare
      Wrap1 : constant Primary_Choise_Lists.List :=
        Primary_Choise_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Choise_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos));
   end;


when  275 =>
--#line  3402
 
yyval := 
yy.value_stack(yy.tos);

when  276 =>
--#line  3404
 
yyval := 
yy.value_stack(yy.tos);

when  277 =>
--#line  3406

   declare
      New_Node : constant Others_Choice_Ptr :=
        New_Others_Choice_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  278 =>
--#line  3420
 
yyval := 
yy.value_stack(yy.tos);

when  279 =>
--#line  3425

   if 
yy.value_stack(yy.tos-3).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Declaration_Ptr :=
           New_Procedure_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
         Set_Has_Abstract (New_Node.all, True);
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Declaration_Ptr :=
           New_Function_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
         Set_Has_Abstract (New_Node.all, True);
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  280 =>
--#line  3455

   if 
yy.value_stack(yy.tos-3).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Declaration_Ptr :=
           New_Procedure_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
         Set_Has_Abstract (New_Node.all, True);
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Declaration_Ptr :=
           New_Function_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
         Set_Has_Abstract (New_Node.all, True);
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  281 =>
--#line  3486

   declare
      New_Node : constant Interface_Type_Ptr :=
        New_Interface_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Has_Synchronized (New_Node.all, True);
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  282 =>
--#line  3499

   declare
      New_Node : constant Interface_Type_Ptr :=
        New_Interface_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Synchronized (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  283 =>
--#line  3511

   declare
      New_Node : constant Interface_Type_Ptr :=
        New_Interface_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Has_Protected (New_Node.all, True);
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  284 =>
--#line  3524

   declare
      New_Node : constant Interface_Type_Ptr :=
        New_Interface_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Protected (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  285 =>
--#line  3536

   declare
      New_Node : constant Interface_Type_Ptr :=
        New_Interface_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Has_Task (New_Node.all, True);
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  286 =>
--#line  3549

   declare
      New_Node : constant Interface_Type_Ptr :=
        New_Interface_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Task (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  287 =>
--#line  3561

   declare
      New_Node : constant Interface_Type_Ptr :=
        New_Interface_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Has_Limited (New_Node.all, True);
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  288 =>
--#line  3574

   declare
      New_Node : constant Interface_Type_Ptr :=
        New_Interface_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Limited (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  289 =>
--#line  3586

   declare
      New_Node : constant Interface_Type_Ptr :=
        New_Interface_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  290 =>
--#line  3598

   declare
      New_Node : constant Interface_Type_Ptr :=
        New_Interface_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  291 =>
--#line  3612

   declare
      Wrap1 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Expression_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  292 =>
--#line  3622

   declare
      Wrap1 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Expression_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos));
   end;


when  293 =>
--#line  3635

   declare
      Wrap1 : constant Access_Type_Ptr :=
        Access_Type_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Null_Exclusion (Wrap1.all, True);
   end;


when  294 =>
--#line  3646

   declare
      Wrap1 : constant Access_Type_Ptr :=
        Access_Type_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
   end;


when  295 =>
--#line  3655

   declare
      Wrap1 : constant Access_Type_Ptr :=
        Access_Type_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Null_Exclusion (Wrap1.all, True);
   end;


when  296 =>
--#line  3666

   declare
      Wrap1 : constant Access_Type_Ptr :=
        Access_Type_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
   end;


when  297 =>
--#line  3678

   declare
      New_Node : constant Access_Type_Ptr :=
        New_Access_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Access_Type_Kind (New_Node.all, Last_Access_Kind);
      Set_Access_To_Object_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  298 =>
--#line  3691

   declare
      New_Node : constant Access_Type_Ptr :=
        New_Access_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Access_To_Object_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  299 =>
--#line  3706

   declare
      Wrap1 : constant Access_Type_Ptr :=
        New_Access_Type_Node (The_Context);
      Wrap2 : constant Primary_Parameter_Lists.List :=
        Primary_Parameter_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Access_Type_Kind (Wrap1.all, An_Access_To_Protected_Procedure);
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Access_To_Subprogram_Parameter_Profile (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  300 =>
--#line  3723

   declare
      Wrap1 : constant Access_Type_Ptr :=
        New_Access_Type_Node (The_Context);
      Wrap2 : constant Primary_Parameter_Lists.List :=
        Primary_Parameter_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Access_To_Subprogram_Parameter_Profile (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  301 =>
--#line  3739

   declare
      New_Node : constant Access_Type_Ptr :=
        New_Access_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Access_Type_Kind (New_Node.all, An_Access_To_Protected_Function);
      Set_Access_To_Function_Result_Subtype (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  302 =>
--#line  3752

   declare
      New_Node : constant Access_Type_Ptr :=
        New_Access_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Access_To_Function_Result_Subtype (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  304 =>
--#line  3771

   declare
      New_Node : constant Anonymous_Access_To_Constant_Ptr :=
        New_Anonymous_Access_To_Constant_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Has_Null_Exclusion (New_Node.all, True);
      Set_Anonymous_Access_To_Object_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  305 =>
--#line  3784

   declare
      New_Node : constant Anonymous_Access_To_Variable_Ptr :=
        New_Anonymous_Access_To_Variable_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Has_Null_Exclusion (New_Node.all, True);
      Set_Anonymous_Access_To_Object_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  306 =>
--#line  3797

   declare
      New_Node : constant Anonymous_Access_To_Constant_Ptr :=
        New_Anonymous_Access_To_Constant_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Anonymous_Access_To_Object_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  307 =>
--#line  3809

   declare
      New_Node : constant Anonymous_Access_To_Variable_Ptr :=
        New_Anonymous_Access_To_Variable_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Anonymous_Access_To_Object_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  308 =>
--#line  3821

   declare
      New_Node : constant Anonymous_Access_To_Protected_Procedure_Ptr :=
        New_Anonymous_Access_To_Protected_Procedure_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Has_Null_Exclusion (New_Node.all, True);
      Set_Access_To_Subprogram_Parameter_Profile (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  309 =>
--#line  3834

   declare
      New_Node : constant Anonymous_Access_To_Procedure_Ptr :=
        New_Anonymous_Access_To_Procedure_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Has_Null_Exclusion (New_Node.all, True);
      Set_Access_To_Subprogram_Parameter_Profile (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  310 =>
--#line  3847

   declare
      New_Node : constant Anonymous_Access_To_Protected_Procedure_Ptr :=
        New_Anonymous_Access_To_Protected_Procedure_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Access_To_Subprogram_Parameter_Profile (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  311 =>
--#line  3859

   declare
      New_Node : constant Anonymous_Access_To_Procedure_Ptr :=
        New_Anonymous_Access_To_Procedure_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Access_To_Subprogram_Parameter_Profile (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  312 =>
--#line  3871

   declare
      New_Node : constant Anonymous_Access_To_Protected_Function_Ptr :=
        New_Anonymous_Access_To_Protected_Function_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Has_Null_Exclusion (New_Node.all, True);
      Set_Access_To_Function_Result_Subtype (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  313 =>
--#line  3884

   declare
      New_Node : constant Anonymous_Access_To_Function_Ptr :=
        New_Anonymous_Access_To_Function_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Has_Null_Exclusion (New_Node.all, True);
      Set_Access_To_Function_Result_Subtype (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  314 =>
--#line  3897

   declare
      New_Node : constant Anonymous_Access_To_Protected_Function_Ptr :=
        New_Anonymous_Access_To_Protected_Function_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Access_To_Function_Result_Subtype (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  315 =>
--#line  3909

   declare
      New_Node : constant Anonymous_Access_To_Function_Ptr :=
        New_Anonymous_Access_To_Function_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Access_To_Function_Result_Subtype (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  316 =>
--#line  3924

   declare
      New_Node : constant Incomplete_Type_Declaration_Ptr :=
        New_Incomplete_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Discriminant_Part (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  317 =>
--#line  3938

   declare
      New_Node : constant Incomplete_Type_Declaration_Ptr :=
        New_Incomplete_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Discriminant_Part (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  318 =>
--#line  3951

   declare
      New_Node : constant Incomplete_Type_Declaration_Ptr :=
        New_Incomplete_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  319 =>
--#line  3964

   declare
      New_Node : constant Incomplete_Type_Declaration_Ptr :=
        New_Incomplete_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  320 =>
--#line  3979

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
   end;


when  321 =>
--#line  3988

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
   end;


when  322 =>
--#line  4000
 
yyval := 
yy.value_stack(yy.tos);

when  323 =>
--#line  4002
 
yyval := 
yy.value_stack(yy.tos);

when  324 =>
--#line  4007
 
yyval := 
yy.value_stack(yy.tos);

when  325 =>
--#line  4009
 
yyval := 
yy.value_stack(yy.tos);

when  326 =>
--#line  4011
 
yyval := 
yy.value_stack(yy.tos);

when  327 =>
--#line  4016
 
yyval := 
yy.value_stack(yy.tos);

when  328 =>
--#line  4018
 
yyval := 
yy.value_stack(yy.tos);

when  329 =>
--#line  4023
 
yyval := 
yy.value_stack(yy.tos);

when  330 =>
--#line  4025
 
yyval := 
yy.value_stack(yy.tos);

when  331 =>
--#line  4027
 
yyval := 
yy.value_stack(yy.tos);

when  332 =>
--#line  4029
 
yyval := 
yy.value_stack(yy.tos);

when  333 =>
--#line  4034
 
yyval := 
yy.value_stack(yy.tos);

when  334 =>
--#line  4036
 
yyval := 
yy.value_stack(yy.tos);

when  335 =>
--#line  4038
 
yyval := 
yy.value_stack(yy.tos);

when  336 =>
--#line  4040
 
yyval := 
yy.value_stack(yy.tos);

when  337 =>
--#line  4042
 
yyval := 
yy.value_stack(yy.tos);

when  338 =>
--#line  4044
 
yyval := 
yy.value_stack(yy.tos);

when  339 =>
--#line  4049
 
yyval := 
yy.value_stack(yy.tos);

when  340 =>
--#line  4051
 
yyval := 
yy.value_stack(yy.tos);

when  341 =>
--#line  4056
 
yyval := 
yy.value_stack(yy.tos);

when  342 =>
--#line  4061

   declare
      New_Node : constant Explicit_Dereference_Ptr :=
        New_Explicit_Dereference_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Prefix (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  343 =>
--#line  4076
 
yyval := 
yy.value_stack(yy.tos);

when  344 =>
--#line  4081
 
yyval := 
yy.value_stack(yy.tos-4);

when  345 =>
--#line  4083
 
yyval := 
yy.value_stack(yy.tos-3);

when  346 =>
--#line  4088
 
yyval := 
yy.value_stack(yy.tos-3);

when  347 =>
--#line  4093

   declare
      New_Node : constant Selected_Component_Ptr :=
        New_Selected_Component_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Prefix (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Selector (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  348 =>
--#line  4109
 
yyval := 
yy.value_stack(yy.tos);

when  349 =>
--#line  4111
 
yyval := 
yy.value_stack(yy.tos);

when  350 =>
--#line  4113
 
yyval := 
yy.value_stack(yy.tos);

when  351 =>
--#line  4118

   declare
      New_Node : constant Attribute_Reference_Ptr :=
        New_Attribute_Reference_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Prefix (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Attribute_Designator_Identifier (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  352 =>
--#line  4134
 
yyval := 
yy.value_stack(yy.tos);

when  353 =>
--#line  4139

   declare
      Wrap1 : constant Range_Attribute_Reference_Ptr :=
        New_Range_Attribute_Reference_Node (The_Context);
      Wrap2 : constant Attribute_Reference_Ptr :=
        Attribute_Reference_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Prefix (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Range_Attribute (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  354 =>
--#line  4158

   declare
      Wrap1 : constant Attribute_Reference_Ptr :=
        New_Attribute_Reference_Node (The_Context);
      Wrap2 : constant Identifier_Ptr :=
        New_Identifier_Node (The_Context);
      Wrap3 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Name_Image (Wrap2.all, To_String (Raw_Image (
yy.value_stack(yy.tos-1).all)));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Primary_Expression_Lists.Add (Wrap3.all, 
yy.value_stack(yy.tos));
      Set_Attribute_Designator_Expressions (Wrap1.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Attribute_Designator_Identifier (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  355 =>
--#line  4179

   declare
      Wrap1 : constant Attribute_Reference_Ptr :=
        New_Attribute_Reference_Node (The_Context);
      Wrap2 : constant Identifier_Ptr :=
        New_Identifier_Node (The_Context);
      Wrap3 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (Wrap2.all, To_String (Raw_Image (
yy.value_stack(yy.tos).all)));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Attribute_Designator_Expressions (Wrap1.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Attribute_Designator_Identifier (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  356 =>
--#line  4202
 
yyval := 
yy.value_stack(yy.tos);

when  357 =>
--#line  4204
 
yyval := 
yy.value_stack(yy.tos);

when  358 =>
--#line  4209

   declare
      New_Node : constant Record_Aggregate_Ptr :=
        New_Record_Aggregate_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Record_Component_Associations (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  359 =>
--#line  4224

   declare
      Wrap1 : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Association_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  360 =>
--#line  4234

   declare
      Wrap1 : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Association_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos));
   end;


when  361 =>
--#line  4244

   declare
      New_Node : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  362 =>
--#line  4258

   declare
      New_Node : constant Record_Component_Association_Ptr :=
        New_Record_Component_Association_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Record_Component_Choices (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Component_Expression (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  363 =>
--#line  4271

   declare
      New_Node : constant Record_Component_Association_Ptr :=
        New_Record_Component_Association_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Component_Expression (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  364 =>
--#line  4283

   declare
      New_Node : constant Record_Component_Association_Ptr :=
        New_Record_Component_Association_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Component_Expression (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  365 =>
--#line  4295

   declare
      Wrap1 : constant Record_Component_Association_Ptr :=
        New_Record_Component_Association_Node (The_Context);
      Wrap2 : constant Box_Expression_Ptr :=
        New_Box_Expression_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Record_Component_Choices (Wrap1.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Component_Expression (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  366 =>
--#line  4316
 
yyval := 
yy.value_stack(yy.tos-1);

when  367 =>
--#line  4318
 
yyval := 
yy.value_stack(yy.tos);

when  369 =>
--#line  4324

   declare
      New_Node : constant Extension_Aggregate_Ptr :=
        New_Extension_Aggregate_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Extension_Aggregate_Expression (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Record_Component_Associations (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  370 =>
--#line  4340
 
yyval := 
yy.value_stack(yy.tos);

when  371 =>
--#line  4345
 
yyval := 
yy.value_stack(yy.tos);

when  372 =>
--#line  4347
 
yyval := 
yy.value_stack(yy.tos);

when  373 =>
--#line  4352
 
yyval := 
yy.value_stack(yy.tos-4);

when  374 =>
--#line  4354
 
yyval := 
yy.value_stack(yy.tos-3);

when  375 =>
--#line  4356
 
yyval := 
yy.value_stack(yy.tos-6);

when  376 =>
--#line  4358
 
yyval := 
yy.value_stack(yy.tos-5);

when  377 =>
--#line  4360
 
yyval := 
yy.value_stack(yy.tos-6);

when  378 =>
--#line  4362
 
yyval := 
yy.value_stack(yy.tos-5);

when  379 =>
--#line  4367
 
yyval := 
yy.value_stack(yy.tos-2);

when  380 =>
--#line  4369
 
yyval := 
yy.value_stack(yy.tos-1);

when  381 =>
--#line  4374
 
yyval := 
yy.value_stack(yy.tos-2);

when  382 =>
--#line  4376
 
yyval := 
yy.value_stack(yy.tos-2);

when  383 =>
--#line  4381
 
yyval := 
yy.value_stack(yy.tos);

when  384 =>
--#line  4383

   declare
      Call_Node1 : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Call_Node2 : constant Function_Call_Ptr :=
        Function_Call_Ptr (
yy.value_stack(yy.tos));
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node1.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Arg_Node1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_End_Position (Arg_Node1.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos-1));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node1.all, T (Arg_List));
      Set_Start_Position (Call_Node1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_End_Position (Call_Node1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-2).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Prefix (Call_Node1.all, T (Sym_Node));
      Set_Is_Prefix_Call (Call_Node1.all, False);
      Push_Argument (Call_Node2.all, T (Call_Node1));
      Set_Start_Position (Call_Node2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      
yyval := YYSTYPE (Call_Node2);
   end;


when  385 =>
--#line  4420

   declare
      Call_Node1 : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node1.all, 
yy.value_stack(yy.tos-2));
      Set_Start_Position (Arg_Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Arg_Node1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node1.all, T (Arg_List));
      Set_Start_Position (Call_Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Call_Node1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node1.all, T (Sym_Node));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-1).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Is_Prefix_Call (Call_Node1.all, False);
      
yyval := YYSTYPE (Call_Node1);
   end;


when  386 =>
--#line  4453

   declare
      Node1 : constant And_Then_Short_Circuit_Ptr:=
        New_And_Then_Short_Circuit_Node (The_Context);
      Node2 : constant And_Then_Short_Circuit_Ptr:=
        And_Then_Short_Circuit_Ptr (
yy.value_stack(yy.tos));
   begin
      Set_Short_Circuit_Operation_Left_Expression (Node1.all, 
yy.value_stack(yy.tos-4));
      Set_Start_Position (Node1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_End_Position (Node1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Short_Circuit_Operation_Right_Expression (Node1.all, 
yy.value_stack(yy.tos-1));
      Push_Argument (Node2.all, T (Node1));
      Set_Start_Position (Node2.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      
yyval := YYSTYPE (Node2);
   end;


when  387 =>
--#line  4470

   declare
      Node1 : constant And_Then_Short_Circuit_Ptr:=
        New_And_Then_Short_Circuit_Node (The_Context);
   begin
      Set_Short_Circuit_Operation_Left_Expression (Node1.all, 
yy.value_stack(yy.tos-3));
      Set_Short_Circuit_Operation_Right_Expression (Node1.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Node1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_End_Position (Node1.all, End_Position (
yy.value_stack(yy.tos).all));
      
yyval := YYSTYPE (Node1);
   end;


when  388 =>
--#line  4483

   declare
      Call_Node1 : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Call_Node2 : constant Function_Call_Ptr :=
        Function_Call_Ptr (
yy.value_stack(yy.tos));
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node1.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Arg_Node1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_End_Position (Arg_Node1.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos-1));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node1.all, T (Arg_List));
      Set_Start_Position (Call_Node1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_End_Position (Call_Node1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-2).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Prefix (Call_Node1.all, T (Sym_Node));
      Set_Is_Prefix_Call (Call_Node1.all, False);
      Push_Argument (Call_Node2.all, T (Call_Node1));
      Set_Start_Position (Call_Node2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      
yyval := YYSTYPE (Call_Node2);
   end;


when  389 =>
--#line  4520

   declare
      Call_Node1 : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node1.all, 
yy.value_stack(yy.tos-2));
      Set_Start_Position (Arg_Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Arg_Node1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node1.all, T (Arg_List));
      Set_Start_Position (Call_Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Call_Node1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node1.all, T (Sym_Node));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-1).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Is_Prefix_Call (Call_Node1.all, False);
      
yyval := YYSTYPE (Call_Node1);
   end;


when  390 =>
--#line  4553

   declare
      Node1 : constant Or_Else_Short_Circuit_Ptr:=
        New_Or_Else_Short_Circuit_Node (The_Context);
      Node2 : constant Or_Else_Short_Circuit_Ptr:=
        Or_Else_Short_Circuit_Ptr (
yy.value_stack(yy.tos));
   begin
      Set_Short_Circuit_Operation_Left_Expression (Node1.all, 
yy.value_stack(yy.tos-4));
      Set_Start_Position (Node1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_End_Position (Node1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Short_Circuit_Operation_Right_Expression (Node1.all, 
yy.value_stack(yy.tos-1));
      Push_Argument (Node2.all, T (Node1));
      Set_Start_Position (Node2.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      
yyval := YYSTYPE (Node2);
   end;


when  391 =>
--#line  4570

   declare
      Node1 : constant Or_Else_Short_Circuit_Ptr:=
        New_Or_Else_Short_Circuit_Node (The_Context);
   begin
      Set_Short_Circuit_Operation_Left_Expression (Node1.all, 
yy.value_stack(yy.tos-3));
      Set_Short_Circuit_Operation_Right_Expression (Node1.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Node1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_End_Position (Node1.all, End_Position (
yy.value_stack(yy.tos).all));
      
yyval := YYSTYPE (Node1);
   end;


when  392 =>
--#line  4583

   declare
      Call_Node1 : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Call_Node2 : constant Function_Call_Ptr :=
        Function_Call_Ptr (
yy.value_stack(yy.tos));
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node1.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Arg_Node1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_End_Position (Arg_Node1.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos-1));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node1.all, T (Arg_List));
      Set_Start_Position (Call_Node1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_End_Position (Call_Node1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-2).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Prefix (Call_Node1.all, T (Sym_Node));
      Set_Is_Prefix_Call (Call_Node1.all, False);
      Push_Argument (Call_Node2.all, T (Call_Node1));
      Set_Start_Position (Call_Node2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      
yyval := YYSTYPE (Call_Node2);
   end;


when  393 =>
--#line  4620

   declare
      Call_Node1 : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node1.all, 
yy.value_stack(yy.tos-2));
      Set_Start_Position (Arg_Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Arg_Node1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node1.all, T (Arg_List));
      Set_Start_Position (Call_Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Call_Node1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node1.all, T (Sym_Node));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-1).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Is_Prefix_Call (Call_Node1.all, False);
      
yyval := YYSTYPE (Call_Node1);
   end;


when  394 =>
--#line  4656

   declare
      Call_Node : constant Function_Call_Ptr :=
        Function_Call_Ptr (
yy.value_stack(yy.tos));
   begin
      Push_Argument (Call_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      
yyval := YYSTYPE (Call_Node);
   end;


when  396 =>
--#line  4668

   declare
      New_Node : constant Not_In_Range_Membership_Test_Ptr :=
        New_Not_In_Range_Membership_Test_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Membership_Test_Expression (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Membership_Test_Range (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  397 =>
--#line  4681

   declare
      New_Node : constant In_Range_Membership_Test_Ptr :=
        New_In_Range_Membership_Test_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Membership_Test_Expression (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Membership_Test_Range (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  398 =>
--#line  4694

   declare
      New_Node : constant Not_In_Type_Membership_Test_Ptr :=
        New_Not_In_Type_Membership_Test_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Membership_Test_Expression (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Membership_Test_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  399 =>
--#line  4707

   declare
      New_Node : constant In_Type_Membership_Test_Ptr :=
        New_In_Type_Membership_Test_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Membership_Test_Expression (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Membership_Test_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  400 =>
--#line  4723

   declare
      Call_Node1 : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Call_Node2 : constant Function_Call_Ptr :=
        Function_Call_Ptr (
yy.value_stack(yy.tos));
   begin
      Set_Actual_Parameter (Arg_Node1.all, 
yy.value_stack(yy.tos-1));
      Set_Start_Position (Arg_Node1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Arg_Node1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Set_Function_Call_Parameters (Call_Node1.all, T (Arg_List));
      Set_Prefix (Call_Node1.all, 
yy.value_stack(yy.tos-2));
      Set_Start_Position (Call_Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Call_Node1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Is_Prefix_Call (Call_Node1.all, False);
      Push_Argument (Call_Node2.all, T (Call_Node1));
      Set_Start_Position (Call_Node2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      
yyval := YYSTYPE (Call_Node2);
   end;


when  401 =>
--#line  4749

   declare
      Call_Node : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List  : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Prefix (Call_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  402 =>
--#line  4771

   declare
      Call_Node : constant Function_Call_Ptr :=
        Function_Call_Ptr (
yy.value_stack(yy.tos));
   begin
      Push_Argument (Call_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      
yyval := YYSTYPE (Call_Node);
   end;


when  404 =>
--#line  4786

   declare
      Call_Node : constant Function_Call_Ptr :=
        Function_Call_Ptr (
yy.value_stack(yy.tos));
   begin
      Push_Argument (Call_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      
yyval := YYSTYPE (Call_Node);
   end;


when  406 =>
--#line  4801

   declare
      Call_Node : constant Function_Call_Ptr :=
        Function_Call_Ptr (
yy.value_stack(yy.tos));
   begin
      Push_Argument (Call_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      
yyval := YYSTYPE (Call_Node);
   end;


when  408 =>
--#line  4813

   declare
      Call_Node  : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node   : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node.all, T (Sym_Node));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-1).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  409 =>
--#line  4842

   declare
      Call_Node  : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node   : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node.all, T (Sym_Node));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-1).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  410 =>
--#line  4874
 
yyval := 
yy.value_stack(yy.tos);

when  411 =>
--#line  4876
 
yyval := 
yy.value_stack(yy.tos);

when  412 =>
--#line  4878

   declare
      New_Node : constant Null_Literal_Ptr :=
        New_Null_Literal_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  413 =>
--#line  4889
 
yyval := 
yy.value_stack(yy.tos);

when  414 =>
--#line  4891
 
yyval := 
yy.value_stack(yy.tos);

when  415 =>
--#line  4893
 
yyval := 
yy.value_stack(yy.tos);

when  416 =>
--#line  4895
 
yyval := 
yy.value_stack(yy.tos);

when  420 =>
--#line  4906

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """=""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  421 =>
--#line  4918

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """/=""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  422 =>
--#line  4930

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """<""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  423 =>
--#line  4942

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """<=""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  424 =>
--#line  4954

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """>""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  425 =>
--#line  4966

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """>=""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  426 =>
--#line  4981

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """+""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  427 =>
--#line  4993

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """-""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  428 =>
--#line  5005

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """&""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  429 =>
--#line  5020

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """+""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  430 =>
--#line  5032

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """-""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  431 =>
--#line  5047

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """*""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  432 =>
--#line  5059

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, """/""");
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  433 =>
--#line  5071

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos).all)) & '"');
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  434 =>
--#line  5083

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos).all)) & '"');
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  438 =>
--#line  5104
 
yyval := 
yy.value_stack(yy.tos-3);

when  439 =>
--#line  5106
 
yyval := 
yy.value_stack(yy.tos-3);

when  440 =>
--#line  5111

   declare
      New_Node : constant Qualified_Expression_Ptr :=
        New_Qualified_Expression_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Converted_Or_Qualified_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Converted_Or_Qualified_Expression (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  441 =>
--#line  5127

   declare
      Wrap1 : constant Allocation_From_Subtype_Ptr :=
        New_Allocation_From_Subtype_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Allocator_Subtype_Indication (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  442 =>
--#line  5145

   declare
      New_Node : constant Allocation_From_Qualified_Expression_Ptr :=
        New_Allocation_From_Qualified_Expression_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Allocator_Qualified_Expression (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  443 =>
--#line  5160

   declare
      Wrap1 : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Statement_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  444 =>
--#line  5170

   declare
      Wrap1 : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Statement_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos));
   end;


when  445 =>
--#line  5183

   Set_Label_Names (Statement_Node (
yy.value_stack(yy.tos).all), 
yy.value_stack(yy.tos-1));
   
yyval := 
yy.value_stack(yy.tos);


when  447 =>
--#line  5189

   Set_Label_Names (Statement_Node (
yy.value_stack(yy.tos).all), 
yy.value_stack(yy.tos-1));
   
yyval := 
yy.value_stack(yy.tos);


when  449 =>
--#line  5198
 
yyval := 
yy.value_stack(yy.tos);

when  450 =>
--#line  5200
 
yyval := 
yy.value_stack(yy.tos);

when  451 =>
--#line  5202
 
yyval := 
yy.value_stack(yy.tos);

when  452 =>
--#line  5204
 
yyval := 
yy.value_stack(yy.tos);

when  453 =>
--#line  5206
 
yyval := 
yy.value_stack(yy.tos);

when  454 =>
--#line  5208
 
yyval := 
yy.value_stack(yy.tos);

when  455 =>
--#line  5210
 
yyval := 
yy.value_stack(yy.tos);

when  456 =>
--#line  5212
 
yyval := 
yy.value_stack(yy.tos);

when  457 =>
--#line  5214
 
yyval := 
yy.value_stack(yy.tos);

when  458 =>
--#line  5216
 
yyval := 
yy.value_stack(yy.tos);

when  459 =>
--#line  5218
 
yyval := 
yy.value_stack(yy.tos);

when  460 =>
--#line  5220
 
yyval := 
yy.value_stack(yy.tos);

when  461 =>
--#line  5225
 
yyval := 
yy.value_stack(yy.tos);

when  462 =>
--#line  5227
 
yyval := 
yy.value_stack(yy.tos);

when  463 =>
--#line  5229
 
yyval := 
yy.value_stack(yy.tos);

when  464 =>
--#line  5231
 
yyval := 
yy.value_stack(yy.tos);

when  465 =>
--#line  5233
 
yyval := 
yy.value_stack(yy.tos);

when  466 =>
--#line  5235
 
yyval := 
yy.value_stack(yy.tos);

when  467 =>
--#line  5237
 
yyval := 
yy.value_stack(yy.tos);

when  468 =>
--#line  5242

   declare
      New_Node : constant Null_Statement_Ptr :=
        New_Null_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  469 =>
--#line  5256
 
yyval := 
yy.value_stack(yy.tos-1);

when  470 =>
--#line  5261
 
yyval := 
yy.value_stack(yy.tos);

when  471 =>
--#line  5266

   declare
      New_Node : constant Assignment_Statement_Ptr :=
        New_Assignment_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Assignment_Variable_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Assignment_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  472 =>
--#line  5282

   declare
      Wrap1 : constant If_Statement_Ptr :=
        New_If_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-4));
      Wrap3 : constant If_Path_Ptr :=
        New_If_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Condition_Expression (Wrap3.all, 
yy.value_stack(yy.tos-7));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-5).all));
      Primary_Path_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  473 =>
--#line  5305

   declare
      Wrap1 : constant If_Statement_Ptr :=
        New_If_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-3));
      Wrap3 : constant If_Path_Ptr :=
        New_If_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Condition_Expression (Wrap3.all, 
yy.value_stack(yy.tos-6));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-4).all));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  474 =>
--#line  5327

   declare
      Wrap1 : constant If_Statement_Ptr :=
        New_If_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant If_Path_Ptr :=
        New_If_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Condition_Expression (Wrap3.all, 
yy.value_stack(yy.tos-6));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-4).all));
      Primary_Path_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  475 =>
--#line  5350

   declare
      Wrap1 : constant If_Statement_Ptr :=
        New_If_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant If_Path_Ptr :=
        New_If_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Condition_Expression (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  476 =>
--#line  5375
 
yyval := 
yy.value_stack(yy.tos);

when  477 =>
--#line  5380

   declare
      Wrap1 : constant Case_Statement_Ptr :=
        New_Case_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-3));
      Wrap3 : constant Case_Path_Ptr :=
        Case_Path_Ptr (
yy.value_stack(yy.tos-4));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Case_Expression (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Pragmas (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  478 =>
--#line  5400

   declare
      Wrap1 : constant Case_Statement_Ptr :=
        New_Case_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant Case_Path_Ptr :=
        Case_Path_Ptr (
yy.value_stack(yy.tos-3));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Case_Expression (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Pragmas (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  479 =>
--#line  5420

   declare
      Wrap1 : constant Case_Statement_Ptr :=
        New_Case_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-3));
      Wrap3 : constant Case_Path_Ptr :=
        Case_Path_Ptr (
yy.value_stack(yy.tos-4));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Case_Expression (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  480 =>
--#line  5438

   declare
      Wrap1 : constant Case_Statement_Ptr :=
        New_Case_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant Case_Path_Ptr :=
        Case_Path_Ptr (
yy.value_stack(yy.tos-3));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Case_Expression (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  481 =>
--#line  5459

   declare
      New_Node : constant Case_Path_Ptr :=
        New_Case_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Case_Statement_Alternative_Choices (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Sequence_Of_Statements (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  482 =>
--#line  5475

   if 
yy.value_stack(yy.tos-6).all in Expression_Node'Class then
      declare
         New_Node : constant While_Loop_Statement_Ptr :=
           New_While_Loop_Statement_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
         Set_Statement_Identifier (New_Node.all, 
yy.value_stack(yy.tos-7));
         Set_While_Condition (New_Node.all, 
yy.value_stack(yy.tos-6));
         Set_Loop_Statements (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Back_Identifier (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant For_Loop_Statement_Ptr :=
           New_For_Loop_Statement_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
         Set_Statement_Identifier (New_Node.all, 
yy.value_stack(yy.tos-7));
         Set_Loop_Parameter_Specification (New_Node.all, 
yy.value_stack(yy.tos-6));
         Set_Loop_Statements (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Back_Identifier (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  483 =>
--#line  5505

   if 
yy.value_stack(yy.tos-5).all in Expression_Node'Class then
      declare
         New_Node : constant While_Loop_Statement_Ptr :=
           New_While_Loop_Statement_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
         Set_Statement_Identifier (New_Node.all, 
yy.value_stack(yy.tos-6));
         Set_While_Condition (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Loop_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant For_Loop_Statement_Ptr :=
           New_For_Loop_Statement_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
         Set_Statement_Identifier (New_Node.all, 
yy.value_stack(yy.tos-6));
         Set_Loop_Parameter_Specification (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Loop_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  484 =>
--#line  5533

   declare
      New_Node : constant Loop_Statement_Ptr :=
        New_Loop_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Statement_Identifier (New_Node.all, 
yy.value_stack(yy.tos-6));
      Set_Loop_Statements (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Back_Identifier (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  485 =>
--#line  5547

   declare
      New_Node : constant Loop_Statement_Ptr :=
        New_Loop_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Statement_Identifier (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Loop_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  486 =>
--#line  5560

   if 
yy.value_stack(yy.tos-6).all in Expression_Node'Class then
      declare
         New_Node : constant While_Loop_Statement_Ptr :=
           New_While_Loop_Statement_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
         Set_While_Condition (New_Node.all, 
yy.value_stack(yy.tos-6));
         Set_Loop_Statements (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Back_Identifier (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant For_Loop_Statement_Ptr :=
           New_For_Loop_Statement_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
         Set_Loop_Parameter_Specification (New_Node.all, 
yy.value_stack(yy.tos-6));
         Set_Loop_Statements (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Back_Identifier (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  487 =>
--#line  5588

   if 
yy.value_stack(yy.tos-5).all in Expression_Node'Class then
      declare
         New_Node : constant While_Loop_Statement_Ptr :=
           New_While_Loop_Statement_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
         Set_While_Condition (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Loop_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant For_Loop_Statement_Ptr :=
           New_For_Loop_Statement_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
         Set_Loop_Parameter_Specification (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Loop_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  488 =>
--#line  5614

   declare
      New_Node : constant Loop_Statement_Ptr :=
        New_Loop_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Loop_Statements (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Back_Identifier (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  489 =>
--#line  5627

   declare
      New_Node : constant Loop_Statement_Ptr :=
        New_Loop_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Loop_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  490 =>
--#line  5642
 
yyval := 
yy.value_stack(yy.tos);

when  491 =>
--#line  5644
 
yyval := 
yy.value_stack(yy.tos);

when  492 =>
--#line  5649

   declare
      New_Node : constant Loop_Parameter_Specification_Ptr :=
        New_Loop_Parameter_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Trait_Kind (New_Node.all, A_Reverse_Trait);
      Set_Specification_Subtype_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  493 =>
--#line  5663

   declare
      New_Node : constant Loop_Parameter_Specification_Ptr :=
        New_Loop_Parameter_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Specification_Subtype_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  494 =>
--#line  5679

   declare
      New_Node : constant Block_Statement_Ptr :=
        New_Block_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Statement_Identifier (New_Node.all, 
yy.value_stack(yy.tos-6));
      Set_Block_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Back_Identifier (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  495 =>
--#line  5694

   declare
      New_Node : constant Block_Statement_Ptr :=
        New_Block_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Statement_Identifier (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Block_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  496 =>
--#line  5708

   declare
      New_Node : constant Block_Statement_Ptr :=
        New_Block_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Statement_Identifier (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Back_Identifier (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  497 =>
--#line  5722

   declare
      New_Node : constant Block_Statement_Ptr :=
        New_Block_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Statement_Identifier (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  498 =>
--#line  5735

   declare
      New_Node : constant Block_Statement_Ptr :=
        New_Block_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Block_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Back_Identifier (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  499 =>
--#line  5749

   declare
      New_Node : constant Block_Statement_Ptr :=
        New_Block_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Block_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  500 =>
--#line  5762

   declare
      New_Node : constant Block_Statement_Ptr :=
        New_Block_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Back_Identifier (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  501 =>
--#line  5775

   declare
      New_Node : constant Block_Statement_Ptr :=
        New_Block_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  502 =>
--#line  5790

   declare
      New_Node : constant Exit_Statement_Ptr :=
        New_Exit_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Exit_Loop_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Exit_Condition (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  503 =>
--#line  5803

   declare
      New_Node : constant Exit_Statement_Ptr :=
        New_Exit_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Exit_Loop_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  504 =>
--#line  5815

   declare
      New_Node : constant Exit_Statement_Ptr :=
        New_Exit_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Exit_Condition (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  505 =>
--#line  5827

   declare
      New_Node : constant Exit_Statement_Ptr :=
        New_Exit_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  506 =>
--#line  5841

   declare
      New_Node : constant Goto_Statement_Ptr :=
        New_Goto_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Goto_Label (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  507 =>
--#line  5856

   if 
yy.value_stack(yy.tos-1).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Declaration_Ptr :=
           New_Procedure_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Declaration_Ptr :=
           New_Function_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  508 =>
--#line  5882

   if 
yy.value_stack(yy.tos-1).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Declaration_Ptr :=
           New_Procedure_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Declaration_Ptr :=
           New_Function_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  509 =>
--#line  5909
 
yyval := 
yy.value_stack(yy.tos);

when  510 =>
--#line  5911
 
yyval := 
yy.value_stack(yy.tos);

when  511 =>
--#line  5916

   declare
      New_Node : constant Procedure_Specification_Ptr :=
        New_Procedure_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Profile (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  512 =>
--#line  5932

   declare
      New_Node : constant Function_Specification_Ptr :=
        New_Function_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Profile (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  513 =>
--#line  5945

   declare
      New_Node : constant Function_Specification_Ptr :=
        New_Function_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  514 =>
--#line  5960
 
yyval := 
yy.value_stack(yy.tos);

when  515 =>
--#line  5962
 
yyval := 
yy.value_stack(yy.tos);

when  516 =>
--#line  5967
 
yyval := 
yy.value_stack(yy.tos);

when  517 =>
--#line  5969
 
yyval := 
yy.value_stack(yy.tos);

when  518 =>
--#line  5974
 
yyval := 
yy.value_stack(yy.tos);

when  519 =>
--#line  5979

   declare
      New_Node : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (New_Node.all, Value_Image (String_Literal_Ptr (
yy.value_stack(yy.tos)).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  520 =>
--#line  5994

   declare
      New_Node : constant Defining_Operator_Symbol_Ptr :=
        New_Defining_Operator_Symbol_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Defining_Name_Image (New_Node.all, Value_Image (String_Literal_Ptr (
yy.value_stack(yy.tos)).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  521 =>
--#line  6009

   declare
      Wrap1 : constant Primary_Parameter_Lists.List :=
        Primary_Parameter_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
   end;


when  522 =>
--#line  6018

   declare
      Wrap1 : constant Primary_Parameter_Lists.List :=
        Primary_Parameter_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
   end;


when  523 =>
--#line  6030

   declare
      Wrap1 : constant Function_Profile_Ptr :=
        New_Function_Profile_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Parameter_Profile (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Null_Exclusion (Wrap2.all, True);
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Result_Profile (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  524 =>
--#line  6050

   declare
      Wrap1 : constant Function_Profile_Ptr :=
        New_Function_Profile_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Parameter_Profile (Wrap1.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Result_Profile (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  525 =>
--#line  6069

   declare
      Wrap1 : constant Function_Profile_Ptr :=
        New_Function_Profile_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Null_Exclusion (Wrap2.all, True);
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Result_Profile (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  526 =>
--#line  6088

   declare
      Wrap1 : constant Function_Profile_Ptr :=
        New_Function_Profile_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Result_Profile (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  527 =>
--#line  6106

   declare
      New_Node : constant Function_Profile_Ptr :=
        New_Function_Profile_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Parameter_Profile (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Result_Profile (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  528 =>
--#line  6119

   declare
      New_Node : constant Function_Profile_Ptr :=
        New_Function_Profile_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Result_Profile (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  529 =>
--#line  6134

   declare
      Wrap1 : constant Primary_Parameter_Lists.List :=
        Primary_Parameter_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Parameter_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-2));
   end;


when  530 =>
--#line  6144

   declare
      Wrap1 : constant Primary_Parameter_Lists.List :=
        Primary_Parameter_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Parameter_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  531 =>
--#line  6157

   declare
      Wrap1 : constant Parameter_Specification_Ptr :=
        New_Parameter_Specification_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Mode_Kind (Wrap1.all, Modes.
yy.value_stack(yy.tos-3));
      Set_Has_Null_Exclusion (Wrap1.all, True);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Initialization_Expression (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  532 =>
--#line  6178

   declare
      Wrap1 : constant Parameter_Specification_Ptr :=
        New_Parameter_Specification_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Mode_Kind (Wrap1.all, Modes.
yy.value_stack(yy.tos-2));
      Set_Has_Null_Exclusion (Wrap1.all, True);
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  533 =>
--#line  6199

   declare
      Wrap1 : constant Parameter_Specification_Ptr :=
        New_Parameter_Specification_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Mode_Kind (Wrap1.all, Modes.
yy.value_stack(yy.tos-2));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Initialization_Expression (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  534 =>
--#line  6219

   declare
      Wrap1 : constant Parameter_Specification_Ptr :=
        New_Parameter_Specification_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Mode_Kind (Wrap1.all, Modes.
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  535 =>
--#line  6239

   declare
      Wrap1 : constant Parameter_Specification_Ptr :=
        New_Parameter_Specification_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Has_Null_Exclusion (Wrap1.all, True);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Initialization_Expression (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  536 =>
--#line  6259

   declare
      Wrap1 : constant Parameter_Specification_Ptr :=
        New_Parameter_Specification_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Null_Exclusion (Wrap1.all, True);
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  537 =>
--#line  6279

   declare
      Wrap1 : constant Parameter_Specification_Ptr :=
        New_Parameter_Specification_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Initialization_Expression (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  538 =>
--#line  6298

   declare
      Wrap1 : constant Parameter_Specification_Ptr :=
        New_Parameter_Specification_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  539 =>
--#line  6317

   declare
      New_Node : constant Parameter_Specification_Ptr :=
        New_Parameter_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Trait_Kind (New_Node.all, An_Access_Definition_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  540 =>
--#line  6332

   declare
      New_Node : constant Parameter_Specification_Ptr :=
        New_Parameter_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, An_Access_Definition_Trait);
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  541 =>
--#line  6349

   if 
yy.value_stack(yy.tos-7).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Body_Declaration_Ptr :=
           New_Procedure_Body_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-8).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-7));
         Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Compound_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Body_Declaration_Ptr :=
           New_Function_Body_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-8).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-7));
         Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Compound_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  542 =>
--#line  6381

   if 
yy.value_stack(yy.tos-6).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Body_Declaration_Ptr :=
           New_Procedure_Body_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-6));
         Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Body_Declaration_Ptr :=
           New_Function_Body_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-6));
         Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  543 =>
--#line  6411

   if 
yy.value_stack(yy.tos-7).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Body_Declaration_Ptr :=
           New_Procedure_Body_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-7));
         Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Compound_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Body_Declaration_Ptr :=
           New_Function_Body_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-7));
         Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Compound_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  544 =>
--#line  6441

   if 
yy.value_stack(yy.tos-6).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Body_Declaration_Ptr :=
           New_Procedure_Body_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-6));
         Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Body_Declaration_Ptr :=
           New_Function_Body_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-6));
         Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  545 =>
--#line  6472
 
yyval := 
yy.value_stack(yy.tos-1);

when  546 =>
--#line  6474
 
yyval := 
yy.value_stack(yy.tos-2);

when  547 =>
--#line  6479

   declare
      New_Node : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Prefix (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Record_Aggregate (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  548 =>
--#line  6495
 
yyval := 
yy.value_stack(yy.tos-2);

when  549 =>
--#line  6497
 
yyval := 
yy.value_stack(yy.tos-1);

when  550 =>
--#line  6502
 
yyval := 
yy.value_stack(yy.tos);

when  551 =>
--#line  6504
 
yyval := 
yy.value_stack(yy.tos);

when  552 =>
--#line  6509
 
yyval := 
yy.value_stack(yy.tos);

when  553 =>
--#line  6511
 
yyval := 
yy.value_stack(yy.tos);

when  554 =>
--#line  6516

   declare
      New_Node : constant Simple_Return_Statement_Ptr :=
        New_Simple_Return_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Return_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  555 =>
--#line  6528

   declare
      New_Node : constant Simple_Return_Statement_Ptr :=
        New_Simple_Return_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  556 =>
--#line  6542

   declare
      Wrap1 : constant Extended_Return_Statement_Ptr :=
        New_Extended_Return_Statement_Node (The_Context);
      Wrap2 : constant Return_Object_Specification_Ptr :=
        New_Return_Object_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (Wrap2.all, 
yy.value_stack(yy.tos-6));
      Set_Trait_Kind (Wrap2.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Initialization_Expression (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Handled_Statements (Wrap1.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Return_Object_Specification (Wrap1.all, T (Wrap2));
   end;


when  557 =>
--#line  6563

   declare
      Wrap1 : constant Extended_Return_Statement_Ptr :=
        New_Extended_Return_Statement_Node (The_Context);
      Wrap2 : constant Return_Object_Specification_Ptr :=
        New_Return_Object_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (Wrap2.all, 
yy.value_stack(yy.tos-5));
      Set_Trait_Kind (Wrap2.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Return_Object_Specification (Wrap1.all, T (Wrap2));
   end;


when  558 =>
--#line  6583

   declare
      Wrap1 : constant Extended_Return_Statement_Ptr :=
        New_Extended_Return_Statement_Node (The_Context);
      Wrap2 : constant Return_Object_Specification_Ptr :=
        New_Return_Object_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (Wrap2.all, 
yy.value_stack(yy.tos-5));
      Set_Trait_Kind (Wrap2.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Handled_Statements (Wrap1.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Return_Object_Specification (Wrap1.all, T (Wrap2));
   end;


when  559 =>
--#line  6603

   declare
      Wrap1 : constant Extended_Return_Statement_Ptr :=
        New_Extended_Return_Statement_Node (The_Context);
      Wrap2 : constant Return_Object_Specification_Ptr :=
        New_Return_Object_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Trait_Kind (Wrap2.all, An_Aliased_Trait);
      Set_Object_Declaration_Subtype (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Return_Object_Specification (Wrap1.all, T (Wrap2));
   end;


when  560 =>
--#line  6622

   declare
      Wrap1 : constant Extended_Return_Statement_Ptr :=
        New_Extended_Return_Statement_Node (The_Context);
      Wrap2 : constant Return_Object_Specification_Ptr :=
        New_Return_Object_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (Wrap2.all, 
yy.value_stack(yy.tos-5));
      Set_Object_Declaration_Subtype (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Initialization_Expression (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Handled_Statements (Wrap1.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Return_Object_Specification (Wrap1.all, T (Wrap2));
   end;


when  561 =>
--#line  6642

   declare
      Wrap1 : constant Extended_Return_Statement_Ptr :=
        New_Extended_Return_Statement_Node (The_Context);
      Wrap2 : constant Return_Object_Specification_Ptr :=
        New_Return_Object_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Object_Declaration_Subtype (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Return_Object_Specification (Wrap1.all, T (Wrap2));
   end;


when  562 =>
--#line  6661

   declare
      Wrap1 : constant Extended_Return_Statement_Ptr :=
        New_Extended_Return_Statement_Node (The_Context);
      Wrap2 : constant Return_Object_Specification_Ptr :=
        New_Return_Object_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Object_Declaration_Subtype (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Handled_Statements (Wrap1.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Return_Object_Specification (Wrap1.all, T (Wrap2));
   end;


when  563 =>
--#line  6680

   declare
      Wrap1 : constant Extended_Return_Statement_Ptr :=
        New_Extended_Return_Statement_Node (The_Context);
      Wrap2 : constant Return_Object_Specification_Ptr :=
        New_Return_Object_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Name (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Object_Declaration_Subtype (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Return_Object_Specification (Wrap1.all, T (Wrap2));
   end;


when  564 =>
--#line  6701
 
yyval := 
yy.value_stack(yy.tos);

when  565 =>
--#line  6703
 
yyval := 
yy.value_stack(yy.tos);

when  566 =>
--#line  6708

   if 
yy.value_stack(yy.tos-3).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Declaration_Ptr :=
           New_Procedure_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Is_Null_Procedure (New_Node.all, True);
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Declaration_Ptr :=
           New_Function_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Is_Null_Procedure (New_Node.all, True);
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  567 =>
--#line  6736

   if 
yy.value_stack(yy.tos-3).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Declaration_Ptr :=
           New_Procedure_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Is_Null_Procedure (New_Node.all, True);
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Declaration_Ptr :=
           New_Function_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Is_Null_Procedure (New_Node.all, True);
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  568 =>
--#line  6765

   declare
      New_Node : constant Package_Declaration_Ptr :=
        New_Package_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Package_Specification (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  569 =>
--#line  6780

   declare
      New_Node : constant Package_Specification_Ptr :=
        New_Package_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Visible_Part_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Private_Part_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Compound_Name (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  570 =>
--#line  6795

   declare
      New_Node : constant Package_Specification_Ptr :=
        New_Package_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Visible_Part_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Private_Part_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  571 =>
--#line  6809

   declare
      New_Node : constant Package_Specification_Ptr :=
        New_Package_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Visible_Part_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Compound_Name (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  572 =>
--#line  6823

   declare
      New_Node : constant Package_Specification_Ptr :=
        New_Package_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Visible_Part_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  573 =>
--#line  6836

   declare
      New_Node : constant Package_Specification_Ptr :=
        New_Package_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Private_Part_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Compound_Name (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  574 =>
--#line  6850

   declare
      New_Node : constant Package_Specification_Ptr :=
        New_Package_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Private_Part_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  575 =>
--#line  6863

   declare
      New_Node : constant Package_Specification_Ptr :=
        New_Package_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Compound_Name (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  576 =>
--#line  6876

   declare
      New_Node : constant Package_Specification_Ptr :=
        New_Package_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  577 =>
--#line  6891

   declare
      New_Node : constant Package_Body_Declaration_Ptr :=
        New_Package_Body_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-6));
      Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Compound_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  578 =>
--#line  6906

   declare
      New_Node : constant Package_Body_Declaration_Ptr :=
        New_Package_Body_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  579 =>
--#line  6920

   declare
      New_Node : constant Package_Body_Declaration_Ptr :=
        New_Package_Body_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Compound_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  580 =>
--#line  6934

   declare
      New_Node : constant Package_Body_Declaration_Ptr :=
        New_Package_Body_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  581 =>
--#line  6950

   declare
      Wrap1 : constant Private_Type_Declaration_Ptr :=
        New_Private_Type_Declaration_Node (The_Context);
      Wrap2 : constant Tagged_Private_Type_Definition_Ptr :=
        New_Tagged_Private_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Has_Tagged (Wrap2.all, True);
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  582 =>
--#line  6974

   declare
      Wrap1 : constant Private_Type_Declaration_Ptr :=
        New_Private_Type_Declaration_Node (The_Context);
      Wrap2 : constant Tagged_Private_Type_Definition_Ptr :=
        New_Tagged_Private_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Has_Tagged (Wrap2.all, True);
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  583 =>
--#line  6996

   declare
      Wrap1 : constant Private_Type_Declaration_Ptr :=
        New_Private_Type_Declaration_Node (The_Context);
      Wrap2 : constant Tagged_Private_Type_Definition_Ptr :=
        New_Tagged_Private_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Has_Tagged (Wrap2.all, True);
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  584 =>
--#line  7018

   declare
      Wrap1 : constant Private_Type_Declaration_Ptr :=
        New_Private_Type_Declaration_Node (The_Context);
      Wrap2 : constant Tagged_Private_Type_Definition_Ptr :=
        New_Tagged_Private_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Has_Tagged (Wrap2.all, True);
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  585 =>
--#line  7038

   declare
      Wrap1 : constant Private_Type_Declaration_Ptr :=
        New_Private_Type_Declaration_Node (The_Context);
      Wrap2 : constant Private_Type_Definition_Ptr :=
        New_Private_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  586 =>
--#line  7059

   declare
      Wrap1 : constant Private_Type_Declaration_Ptr :=
        New_Private_Type_Declaration_Node (The_Context);
      Wrap2 : constant Private_Type_Definition_Ptr :=
        New_Private_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  587 =>
--#line  7078

   declare
      Wrap1 : constant Private_Type_Declaration_Ptr :=
        New_Private_Type_Declaration_Node (The_Context);
      Wrap2 : constant Tagged_Private_Type_Definition_Ptr :=
        New_Tagged_Private_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Has_Tagged (Wrap2.all, True);
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  588 =>
--#line  7101

   declare
      Wrap1 : constant Private_Type_Declaration_Ptr :=
        New_Private_Type_Declaration_Node (The_Context);
      Wrap2 : constant Tagged_Private_Type_Definition_Ptr :=
        New_Tagged_Private_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Has_Tagged (Wrap2.all, True);
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  589 =>
--#line  7122

   declare
      Wrap1 : constant Private_Type_Declaration_Ptr :=
        New_Private_Type_Declaration_Node (The_Context);
      Wrap2 : constant Tagged_Private_Type_Definition_Ptr :=
        New_Tagged_Private_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Has_Tagged (Wrap2.all, True);
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  590 =>
--#line  7143

   declare
      Wrap1 : constant Private_Type_Declaration_Ptr :=
        New_Private_Type_Declaration_Node (The_Context);
      Wrap2 : constant Tagged_Private_Type_Definition_Ptr :=
        New_Tagged_Private_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Has_Tagged (Wrap2.all, True);
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  591 =>
--#line  7162

   declare
      Wrap1 : constant Private_Type_Declaration_Ptr :=
        New_Private_Type_Declaration_Node (The_Context);
      Wrap2 : constant Private_Type_Definition_Ptr :=
        New_Private_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  592 =>
--#line  7182

   declare
      Wrap1 : constant Private_Type_Declaration_Ptr :=
        New_Private_Type_Declaration_Node (The_Context);
      Wrap2 : constant Private_Type_Definition_Ptr :=
        New_Private_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  593 =>
--#line  7203

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-11).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-10));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-9));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Has_Synchronized (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  594 =>
--#line  7227

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-10).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-9));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Has_Synchronized (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  595 =>
--#line  7250

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-11).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-10));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-9));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  596 =>
--#line  7275

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-10).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-9));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  597 =>
--#line  7299

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-10).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-9));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  598 =>
--#line  7322

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-9).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  599 =>
--#line  7344

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-10).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-9));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Has_Synchronized (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  600 =>
--#line  7366

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-9).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Has_Synchronized (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  601 =>
--#line  7387

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-10).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-9));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  602 =>
--#line  7410

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-9).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  603 =>
--#line  7432

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-9).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  604 =>
--#line  7453

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Discriminant_Part (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  605 =>
--#line  7473

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-10).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-9));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Has_Synchronized (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  606 =>
--#line  7496

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-9).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Has_Synchronized (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  607 =>
--#line  7518

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-10).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-9));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  608 =>
--#line  7542

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-9).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  609 =>
--#line  7565

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-9).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  610 =>
--#line  7587

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Trait_Kind (Wrap2.all, An_Abstract_Trait);
      Set_Has_Abstract (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  611 =>
--#line  7608

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-9).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Has_Synchronized (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  612 =>
--#line  7629

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Has_Synchronized (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  613 =>
--#line  7649

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-9).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-8));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  614 =>
--#line  7671

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Trait_Kind (Wrap2.all, A_Limited_Trait);
      Set_Has_Limited (Wrap2.all, True);
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  615 =>
--#line  7692

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  616 =>
--#line  7712

   declare
      Wrap1 : constant Private_Extension_Declaration_Ptr :=
        New_Private_Extension_Declaration_Node (The_Context);
      Wrap2 : constant Private_Extension_Definition_Ptr :=
        New_Private_Extension_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Name (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Ancestor_Subtype_Indication (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Has_Private (Wrap2.all, True);
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Type_Declaration_View (Wrap1.all, T (Wrap2));
   end;


when  617 =>
--#line  7734
 
yyval := 
yy.value_stack(yy.tos);

when  618 =>
--#line  7736
 
yyval := 
yy.value_stack(yy.tos);

when  619 =>
--#line  7741

   declare
      Wrap1 : constant Use_Package_Clause_Ptr :=
        New_Use_Package_Clause_Node (The_Context);
      Wrap2 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Primary_Expression_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Clause_Names (Wrap1.all, T (Wrap2));
   end;


when  620 =>
--#line  7756

   declare
      Wrap1 : constant Use_Package_Clause_Ptr :=
        New_Use_Package_Clause_Node (The_Context);
      Wrap2 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Primary_Expression_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Clause_Names (Wrap1.all, T (Wrap2));
   end;


when  621 =>
--#line  7774

   declare
      Wrap1 : constant Use_Type_Clause_Ptr :=
        New_Use_Type_Clause_Node (The_Context);
      Wrap2 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Primary_Expression_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Clause_Names (Wrap1.all, T (Wrap2));
   end;


when  622 =>
--#line  7789

   declare
      Wrap1 : constant Use_Type_Clause_Ptr :=
        New_Use_Type_Clause_Node (The_Context);
      Wrap2 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Primary_Expression_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Clause_Names (Wrap1.all, T (Wrap2));
   end;


when  623 =>
--#line  7807
 
yyval := 
yy.value_stack(yy.tos);

when  624 =>
--#line  7809
 
yyval := 
yy.value_stack(yy.tos);

when  625 =>
--#line  7811
 
yyval := 
yy.value_stack(yy.tos);

when  626 =>
--#line  7813
 
yyval := 
yy.value_stack(yy.tos);

when  627 =>
--#line  7815
 
yyval := 
yy.value_stack(yy.tos);

when  628 =>
--#line  7820

   declare
      Wrap1 : constant Object_Renaming_Declaration_Ptr :=
        New_Object_Renaming_Declaration_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Set_Has_Null_Exclusion (Wrap1.all, True);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Renamed_Entity (Wrap1.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  629 =>
--#line  7840

   declare
      Wrap1 : constant Object_Renaming_Declaration_Ptr :=
        New_Object_Renaming_Declaration_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Renamed_Entity (Wrap1.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  630 =>
--#line  7859

   declare
      New_Node : constant Object_Renaming_Declaration_Ptr :=
        New_Object_Renaming_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Renamed_Entity (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  631 =>
--#line  7876

   declare
      New_Node : constant Exception_Renaming_Declaration_Ptr :=
        New_Exception_Renaming_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Renamed_Entity (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  632 =>
--#line  7892

   declare
      New_Node : constant Package_Renaming_Declaration_Ptr :=
        New_Package_Renaming_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Renamed_Entity (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  633 =>
--#line  7908

   if 
yy.value_stack(yy.tos-3).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Renaming_Declaration_Ptr :=
           New_Procedure_Renaming_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Renamed_Entity (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Renaming_Declaration_Ptr :=
           New_Function_Renaming_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Renamed_Entity (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  634 =>
--#line  7936

   if 
yy.value_stack(yy.tos-3).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Renaming_Declaration_Ptr :=
           New_Procedure_Renaming_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Renamed_Entity (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Renaming_Declaration_Ptr :=
           New_Function_Renaming_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Renamed_Entity (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  635 =>
--#line  7965

   declare
      New_Node : constant Generic_Package_Renaming_Declaration_Ptr :=
        New_Generic_Package_Renaming_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Empty_Generic_Part (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Renamed_Entity (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  636 =>
--#line  7979

   if 
yy.value_stack(yy.tos-3).all in Procedure_Specification_Node then
      declare
         New_Node : constant Generic_Procedure_Renaming_Declaration_Ptr :=
           New_Generic_Procedure_Renaming_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Empty_Generic_Part (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Renamed_Entity (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Generic_Function_Renaming_Declaration_Ptr :=
           New_Generic_Function_Renaming_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Empty_Generic_Part (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Renamed_Entity (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  637 =>
--#line  8010

   declare
      New_Node : constant Task_Type_Declaration_Ptr :=
        New_Task_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Discriminant_Part (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  638 =>
--#line  8025

   declare
      New_Node : constant Task_Type_Declaration_Ptr :=
        New_Task_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Discriminant_Part (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  639 =>
--#line  8039

   declare
      New_Node : constant Task_Type_Declaration_Ptr :=
        New_Task_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Discriminant_Part (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  640 =>
--#line  8052

   declare
      New_Node : constant Task_Type_Declaration_Ptr :=
        New_Task_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  641 =>
--#line  8066

   declare
      New_Node : constant Task_Type_Declaration_Ptr :=
        New_Task_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  642 =>
--#line  8079

   declare
      New_Node : constant Task_Type_Declaration_Ptr :=
        New_Task_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  643 =>
--#line  8094

   declare
      New_Node : constant Single_Task_Declaration_Ptr :=
        New_Single_Task_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  644 =>
--#line  8108

   declare
      New_Node : constant Single_Task_Declaration_Ptr :=
        New_Single_Task_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  645 =>
--#line  8121

   declare
      New_Node : constant Single_Task_Declaration_Ptr :=
        New_Single_Task_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  646 =>
--#line  8136

   declare
      New_Node : constant Task_Definition_Ptr :=
        New_Task_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Visible_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Private_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Identifier (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  647 =>
--#line  8150

   declare
      New_Node : constant Task_Definition_Ptr :=
        New_Task_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Visible_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Private_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  648 =>
--#line  8163

   declare
      New_Node : constant Task_Definition_Ptr :=
        New_Task_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Visible_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Identifier (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  649 =>
--#line  8176

   declare
      New_Node : constant Task_Definition_Ptr :=
        New_Task_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Visible_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  650 =>
--#line  8188

   declare
      New_Node : constant Task_Definition_Ptr :=
        New_Task_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Private_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Identifier (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  651 =>
--#line  8201

   declare
      New_Node : constant Task_Definition_Ptr :=
        New_Task_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Private_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  652 =>
--#line  8213

   declare
      New_Node : constant Task_Definition_Ptr :=
        New_Task_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Identifier (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  653 =>
--#line  8225

   declare
      New_Node : constant Task_Definition_Ptr :=
        New_Task_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  654 =>
--#line  8239
 
yyval := 
yy.value_stack(yy.tos);

when  655 =>
--#line  8241
 
yyval := 
yy.value_stack(yy.tos);

when  656 =>
--#line  8246

   declare
      New_Node : constant Task_Body_Declaration_Ptr :=
        New_Task_Body_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-9).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-7));
      Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Compound_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  657 =>
--#line  8261

   declare
      New_Node : constant Task_Body_Declaration_Ptr :=
        New_Task_Body_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-6));
      Set_Body_Declarative_Items (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  658 =>
--#line  8278

   declare
      New_Node : constant Protected_Type_Declaration_Ptr :=
        New_Protected_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Discriminant_Part (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  659 =>
--#line  8293

   declare
      New_Node : constant Protected_Type_Declaration_Ptr :=
        New_Protected_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Discriminant_Part (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  660 =>
--#line  8307

   declare
      New_Node : constant Protected_Type_Declaration_Ptr :=
        New_Protected_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  661 =>
--#line  8321

   declare
      New_Node : constant Protected_Type_Declaration_Ptr :=
        New_Protected_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  662 =>
--#line  8337

   declare
      New_Node : constant Single_Protected_Declaration_Ptr :=
        New_Single_Protected_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  663 =>
--#line  8351

   declare
      New_Node : constant Single_Protected_Declaration_Ptr :=
        New_Single_Protected_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  664 =>
--#line  8367

   declare
      New_Node : constant Protected_Definition_Ptr :=
        New_Protected_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Visible_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Private_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Identifier (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  665 =>
--#line  8381

   declare
      New_Node : constant Protected_Definition_Ptr :=
        New_Protected_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Visible_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Private_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  666 =>
--#line  8394

   declare
      New_Node : constant Protected_Definition_Ptr :=
        New_Protected_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Visible_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Identifier (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  667 =>
--#line  8407

   declare
      New_Node : constant Protected_Definition_Ptr :=
        New_Protected_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Visible_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  668 =>
--#line  8419

   declare
      New_Node : constant Protected_Definition_Ptr :=
        New_Protected_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Private_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Identifier (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  669 =>
--#line  8432

   declare
      New_Node : constant Protected_Definition_Ptr :=
        New_Protected_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Private_Part_Items (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  670 =>
--#line  8444

   declare
      New_Node : constant Protected_Definition_Ptr :=
        New_Protected_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Identifier (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  671 =>
--#line  8456

   declare
      New_Node : constant Protected_Definition_Ptr :=
        New_Protected_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  672 =>
--#line  8470
 
yyval := 
yy.value_stack(yy.tos);

when  673 =>
--#line  8472
 
yyval := 
yy.value_stack(yy.tos);

when  674 =>
--#line  8474
 
yyval := 
yy.value_stack(yy.tos);

when  675 =>
--#line  8479
 
yyval := 
yy.value_stack(yy.tos);

when  676 =>
--#line  8481
 
yyval := 
yy.value_stack(yy.tos);

when  677 =>
--#line  8486

   declare
      New_Node : constant Protected_Body_Declaration_Ptr :=
        New_Protected_Body_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Protected_Operation_Items (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Identifier (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  678 =>
--#line  8500

   declare
      New_Node : constant Protected_Body_Declaration_Ptr :=
        New_Protected_Body_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Protected_Operation_Items (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  679 =>
--#line  8513

   declare
      New_Node : constant Protected_Body_Declaration_Ptr :=
        New_Protected_Body_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Identifier (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  680 =>
--#line  8526

   declare
      New_Node : constant Protected_Body_Declaration_Ptr :=
        New_Protected_Body_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  681 =>
--#line  8541
 
yyval := 
yy.value_stack(yy.tos);

when  682 =>
--#line  8543
 
yyval := 
yy.value_stack(yy.tos);

when  683 =>
--#line  8545
 
yyval := 
yy.value_stack(yy.tos);

when  684 =>
--#line  8547
 
yyval := 
yy.value_stack(yy.tos);

when  685 =>
--#line  8552

   declare
      New_Node : constant Entry_Declaration_Ptr :=
        New_Entry_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Entry_Family_Definition (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Parameter_Profile (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  686 =>
--#line  8567

   declare
      New_Node : constant Entry_Declaration_Ptr :=
        New_Entry_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Parameter_Profile (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  687 =>
--#line  8581

   declare
      New_Node : constant Entry_Declaration_Ptr :=
        New_Entry_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Entry_Family_Definition (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Parameter_Profile (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  688 =>
--#line  8595

   declare
      New_Node : constant Entry_Declaration_Ptr :=
        New_Entry_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Parameter_Profile (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  689 =>
--#line  8611

   declare
      New_Node : constant Accept_Statement_Ptr :=
        New_Accept_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Accept_Entry_Direct_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Accept_Entry_Index (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Accept_Parameters (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  690 =>
--#line  8626

   declare
      New_Node : constant Accept_Statement_Ptr :=
        New_Accept_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Accept_Entry_Direct_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Accept_Entry_Index (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Accept_Parameters (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  691 =>
--#line  8640

   declare
      New_Node : constant Accept_Statement_Ptr :=
        New_Accept_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Accept_Entry_Direct_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Accept_Parameters (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Handled_Statements (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  692 =>
--#line  8654

   declare
      New_Node : constant Accept_Statement_Ptr :=
        New_Accept_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Accept_Entry_Direct_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Accept_Parameters (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  693 =>
--#line  8670
 
yyval := 
yy.value_stack(yy.tos);

when  694 =>
--#line  8675

   declare
      Wrap1 : constant Entry_Body_Declaration_Ptr :=
        New_Entry_Body_Declaration_Node (The_Context);
      Wrap2 : constant Function_Specification_Ptr :=
        Function_Specification_Ptr (
yy.value_stack(yy.tos-8));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-10).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-9).all));
      Set_Names (Wrap2.all, 
yy.value_stack(yy.tos-9));
      Set_Entry_Barrier (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Body_Declarative_Items (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Handled_Statements (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Compound_Name (Wrap1.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Specification (Wrap1.all, T (Wrap2));
   end;


when  695 =>
--#line  8695

   declare
      Wrap1 : constant Entry_Body_Declaration_Ptr :=
        New_Entry_Body_Declaration_Node (The_Context);
      Wrap2 : constant Function_Specification_Ptr :=
        Function_Specification_Ptr (
yy.value_stack(yy.tos-7));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-9).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Names (Wrap2.all, 
yy.value_stack(yy.tos-8));
      Set_Entry_Barrier (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Set_Body_Declarative_Items (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Handled_Statements (Wrap1.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Specification (Wrap1.all, T (Wrap2));
   end;


when  696 =>
--#line  8717

   declare
      Wrap1 : constant Function_Specification_Ptr :=
        New_Function_Specification_Node (The_Context);
      Wrap2 : constant Function_Profile_Ptr :=
        New_Function_Profile_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Result_Profile (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_Parameter_Profile (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Profile (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  697 =>
--#line  8735

   declare
      Wrap1 : constant Function_Specification_Ptr :=
        New_Function_Specification_Node (The_Context);
      Wrap2 : constant Function_Profile_Ptr :=
        New_Function_Profile_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Parameter_Profile (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Profile (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  698 =>
--#line  8755
 
yyval := 
yy.value_stack(yy.tos);

when  699 =>
--#line  8760

   declare
      New_Node : constant Entry_Index_Specification_Ptr :=
        New_Entry_Index_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Specification_Subtype_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  700 =>
--#line  8776

   declare
      New_Node : constant Procedure_Call_Statement_Ptr :=
        New_Procedure_Call_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Called_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  701 =>
--#line  8791

   declare
      New_Node : constant Requeue_Statement_With_Abort_Ptr :=
        New_Requeue_Statement_With_Abort_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Requeue_Entry_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  702 =>
--#line  8803

   declare
      New_Node : constant Requeue_Statement_Ptr :=
        New_Requeue_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Requeue_Entry_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  703 =>
--#line  8818
 
yyval := 
yy.value_stack(yy.tos);

when  704 =>
--#line  8820
 
yyval := 
yy.value_stack(yy.tos);

when  705 =>
--#line  8825

   declare
      New_Node : constant Delay_Until_Statement_Ptr :=
        New_Delay_Until_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Delay_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  706 =>
--#line  8840

   declare
      New_Node : constant Delay_Relative_Statement_Ptr :=
        New_Delay_Relative_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Delay_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  707 =>
--#line  8855
 
yyval := 
yy.value_stack(yy.tos);

when  708 =>
--#line  8857
 
yyval := 
yy.value_stack(yy.tos);

when  709 =>
--#line  8862

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-4));
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Guard (Wrap3.all, 
yy.value_stack(yy.tos-7));
      Set_Pragmas (Wrap3.all, 
yy.value_stack(yy.tos-6));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-5).all));
      Primary_Path_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  710 =>
--#line  8886

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-3));
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Guard (Wrap3.all, 
yy.value_stack(yy.tos-6));
      Set_Pragmas (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-4).all));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  711 =>
--#line  8909

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Guard (Wrap3.all, 
yy.value_stack(yy.tos-6));
      Set_Pragmas (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-4).all));
      Primary_Path_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  712 =>
--#line  8933

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Guard (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_Pragmas (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  713 =>
--#line  8956

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-4));
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Guard (Wrap3.all, 
yy.value_stack(yy.tos-6));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-5).all));
      Primary_Path_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  714 =>
--#line  8979

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-3));
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Guard (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-4).all));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  715 =>
--#line  9001

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Guard (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-4).all));
      Primary_Path_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  716 =>
--#line  9024

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Guard (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  717 =>
--#line  9046

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-4));
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Pragmas (Wrap3.all, 
yy.value_stack(yy.tos-6));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-5).all));
      Primary_Path_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  718 =>
--#line  9069

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-3));
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Pragmas (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-4).all));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  719 =>
--#line  9091

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Pragmas (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-4).all));
      Primary_Path_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  720 =>
--#line  9114

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Pragmas (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  721 =>
--#line  9136

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-4));
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-5));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-5).all));
      Primary_Path_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  722 =>
--#line  9158

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-3));
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-4).all));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  723 =>
--#line  9179

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-4));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-4).all));
      Primary_Path_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  724 =>
--#line  9201

   declare
      Wrap1 : constant Selective_Accept_Statement_Ptr :=
        New_Selective_Accept_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  725 =>
--#line  9225
 
yyval := 
yy.value_stack(yy.tos-1);

when  726 =>
--#line  9230
 
yyval := 
yy.value_stack(yy.tos);

when  727 =>
--#line  9232
 
yyval := 
yy.value_stack(yy.tos);

when  728 =>
--#line  9234
 
yyval := 
yy.value_stack(yy.tos);

when  729 =>
--#line  9236
 
yyval := 
yy.value_stack(yy.tos);

when  730 =>
--#line  9241

   declare
      Wrap1 : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Statement_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  731 =>
--#line  9251

   declare
      Wrap1 : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Statement_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos));
   end;


when  732 =>
--#line  9264

   declare
      Wrap1 : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Statement_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  733 =>
--#line  9274

   declare
      Wrap1 : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Statement_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos));
   end;


when  734 =>
--#line  9287

   declare
      Wrap1 : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.List (
yy.value_stack(yy.tos));
      Wrap2 : constant Terminate_Alternative_Statement_Ptr :=
        New_Terminate_Alternative_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Statement_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  735 =>
--#line  9301

   declare
      Wrap1 : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.New_List (The_Context);
      Wrap2 : constant Terminate_Alternative_Statement_Ptr :=
        New_Terminate_Alternative_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Statement_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  736 =>
--#line  9318
 
yyval := 
yy.value_stack(yy.tos-5);

when  737 =>
--#line  9323

   declare
      Wrap1 : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Statement_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  738 =>
--#line  9333

   declare
      Wrap1 : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Statement_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos));
   end;


when  739 =>
--#line  9346
 
yyval := 
yy.value_stack(yy.tos);

when  740 =>
--#line  9351
 
yyval := 
yy.value_stack(yy.tos-5);

when  741 =>
--#line  9356

   declare
      Wrap1 : constant Asynchronous_Select_Statement_Ptr :=
        New_Asynchronous_Select_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
      Wrap4 : constant Then_Abort_Path_Ptr :=
        New_Then_Abort_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-8).all));
      Set_Pragmas (Wrap3.all, 
yy.value_stack(yy.tos-7));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-6));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-6).all));
      Set_Start_Position (Wrap4.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Sequence_Of_Statements (Wrap4.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (Wrap4.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap4));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  742 =>
--#line  9384

   declare
      Wrap1 : constant Asynchronous_Select_Statement_Ptr :=
        New_Asynchronous_Select_Statement_Node (The_Context);
      Wrap2 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap3 : constant Select_Path_Ptr :=
        New_Select_Path_Node (The_Context);
      Wrap4 : constant Then_Abort_Path_Ptr :=
        New_Then_Abort_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Sequence_Of_Statements (Wrap3.all, 
yy.value_stack(yy.tos-6));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-6).all));
      Set_Start_Position (Wrap4.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Sequence_Of_Statements (Wrap4.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (Wrap4.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap4));
      Primary_Path_Lists.Add (Wrap2.all, T (Wrap3));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Statement_Paths (Wrap1.all, T (Wrap2));
   end;


when  743 =>
--#line  9414
 
yyval := 
yy.value_stack(yy.tos-1);

when  744 =>
--#line  9416
 
yyval := 
yy.value_stack(yy.tos);

when  745 =>
--#line  9421
 
yyval := 
yy.value_stack(yy.tos);

when  746 =>
--#line  9423
 
yyval := 
yy.value_stack(yy.tos);

when  747 =>
--#line  9428
 
yyval := 
yy.value_stack(yy.tos);

when  748 =>
--#line  9433

   declare
      Wrap1 : constant Abort_Statement_Ptr :=
        New_Abort_Statement_Node (The_Context);
      Wrap2 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Primary_Expression_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Aborted_Tasks (Wrap1.all, T (Wrap2));
   end;


when  749 =>
--#line  9448

   declare
      Wrap1 : constant Abort_Statement_Ptr :=
        New_Abort_Statement_Node (The_Context);
      Wrap2 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Primary_Expression_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Aborted_Tasks (Wrap1.all, T (Wrap2));
   end;


when  750 =>
--#line  9466

   declare
      Wrap1 : constant Primary_Unit_Lists.List :=
        Primary_Unit_Lists.List (
yy.value_stack(yy.tos));
   begin
      Last_Compilation := T (Wrap1);
   end;


when  751 =>
--#line  9475

   declare
      Wrap1 : constant Primary_Unit_Lists.List :=
        Primary_Unit_Lists.New_List (The_Context);
   begin
      Last_Compilation := T (Wrap1);
   end;


when  752 =>
--#line  9487

   declare
      Wrap1 : constant Any_Compilation_Unit_Ptr :=
        Any_Compilation_Unit_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Context_Clause_Elements (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  753 =>
--#line  9498

   declare
      Wrap1 : constant Any_Compilation_Unit_Ptr :=
        Any_Compilation_Unit_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Context_Clause_Elements (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  754 =>
--#line  9509
 
yyval := 
yy.value_stack(yy.tos);

when  755 =>
--#line  9511

   declare
      Wrap1 : constant Any_Compilation_Unit_Ptr :=
        Any_Compilation_Unit_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
   end;


when  756 =>
--#line  9520

   declare
      Wrap1 : constant Any_Compilation_Unit_Ptr :=
        Any_Compilation_Unit_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
   end;


when  757 =>
--#line  9532

   declare
      New_Node : constant Any_Compilation_Unit_Ptr :=
        Any_Compilation_Unit_Ptr (New_Compilation_Unit (The_Context));
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Unit_Declaration (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  758 =>
--#line  9544

   declare
      New_Node : constant Any_Compilation_Unit_Ptr :=
        Any_Compilation_Unit_Ptr (New_Compilation_Unit (The_Context));
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Unit_Declaration (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  759 =>
--#line  9556

   declare
      New_Node : constant Any_Compilation_Unit_Ptr :=
        Any_Compilation_Unit_Ptr (New_Compilation_Unit (The_Context));
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Unit_Declaration (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  760 =>
--#line  9571
 
yyval := 
yy.value_stack(yy.tos);

when  761 =>
--#line  9573
 
yyval := 
yy.value_stack(yy.tos);

when  762 =>
--#line  9575
 
yyval := 
yy.value_stack(yy.tos);

when  763 =>
--#line  9577
 
yyval := 
yy.value_stack(yy.tos);

when  764 =>
--#line  9582
 
yyval := 
yy.value_stack(yy.tos);

when  765 =>
--#line  9584
 
yyval := 
yy.value_stack(yy.tos);

when  766 =>
--#line  9586
 
yyval := 
yy.value_stack(yy.tos);

when  767 =>
--#line  9591
 
yyval := 
yy.value_stack(yy.tos);

when  768 =>
--#line  9593
 
yyval := 
yy.value_stack(yy.tos);

when  769 =>
--#line  9598
 
yyval := 
yy.value_stack(yy.tos);

when  770 =>
--#line  9603

   declare
      Wrap1 : constant Primary_Clause_Lists.List :=
        Primary_Clause_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Clause_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  771 =>
--#line  9613

   declare
      Wrap1 : constant Primary_Clause_Lists.List :=
        Primary_Clause_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Clause_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos));
   end;


when  772 =>
--#line  9626
 
yyval := 
yy.value_stack(yy.tos);

when  773 =>
--#line  9628
 
yyval := 
yy.value_stack(yy.tos);

when  774 =>
--#line  9630

   declare
      New_Node : constant Private_Indicator_Ptr :=
        New_Private_Indicator_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  775 =>
--#line  9644
 
yyval := 
yy.value_stack(yy.tos);

when  776 =>
--#line  9646
 
yyval := 
yy.value_stack(yy.tos);

when  777 =>
--#line  9651

   declare
      Wrap1 : constant With_Clause_Ptr :=
        New_With_Clause_Node (The_Context);
      Wrap2 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Has_Limited (Wrap1.all, True);
      Set_Has_Private (Wrap1.all, True);
      Primary_Expression_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Clause_Names (Wrap1.all, T (Wrap2));
   end;


when  778 =>
--#line  9668

   declare
      Wrap1 : constant With_Clause_Ptr :=
        New_With_Clause_Node (The_Context);
      Wrap2 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Has_Limited (Wrap1.all, True);
      Set_Has_Private (Wrap1.all, True);
      Primary_Expression_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Clause_Names (Wrap1.all, T (Wrap2));
   end;


when  779 =>
--#line  9685

   declare
      Wrap1 : constant With_Clause_Ptr :=
        New_With_Clause_Node (The_Context);
      Wrap2 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Has_Limited (Wrap1.all, True);
      Primary_Expression_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Clause_Names (Wrap1.all, T (Wrap2));
   end;


when  780 =>
--#line  9701

   declare
      Wrap1 : constant With_Clause_Ptr :=
        New_With_Clause_Node (The_Context);
      Wrap2 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Has_Limited (Wrap1.all, True);
      Primary_Expression_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Clause_Names (Wrap1.all, T (Wrap2));
   end;


when  781 =>
--#line  9720

   declare
      Wrap1 : constant With_Clause_Ptr :=
        New_With_Clause_Node (The_Context);
      Wrap2 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Primary_Expression_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Clause_Names (Wrap1.all, T (Wrap2));
   end;


when  782 =>
--#line  9735

   declare
      Wrap1 : constant With_Clause_Ptr :=
        New_With_Clause_Node (The_Context);
      Wrap2 : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Primary_Expression_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Clause_Names (Wrap1.all, T (Wrap2));
   end;


when  783 =>
--#line  9753
 
yyval := 
yy.value_stack(yy.tos);

when  784 =>
--#line  9755
 
yyval := 
yy.value_stack(yy.tos);

when  785 =>
--#line  9757
 
yyval := 
yy.value_stack(yy.tos);

when  786 =>
--#line  9759
 
yyval := 
yy.value_stack(yy.tos);

when  787 =>
--#line  9764

   if 
yy.value_stack(yy.tos-3).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Body_Stub_Ptr :=
           New_Procedure_Body_Stub_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Body_Stub_Ptr :=
           New_Function_Body_Stub_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  788 =>
--#line  9790

   if 
yy.value_stack(yy.tos-3).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Body_Stub_Ptr :=
           New_Procedure_Body_Stub_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Body_Stub_Ptr :=
           New_Function_Body_Stub_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  789 =>
--#line  9817

   declare
      New_Node : constant Package_Body_Stub_Ptr :=
        New_Package_Body_Stub_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  790 =>
--#line  9832

   declare
      New_Node : constant Task_Body_Stub_Ptr :=
        New_Task_Body_Stub_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  791 =>
--#line  9847

   declare
      New_Node : constant Protected_Body_Stub_Ptr :=
        New_Protected_Body_Stub_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  792 =>
--#line  9862

   declare
      New_Node : constant Any_Compilation_Unit_Ptr :=
        Any_Compilation_Unit_Ptr (New_Compilation_Unit (The_Context));
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Separate_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Unit_Declaration (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  793 =>
--#line  9878

   declare
      New_Node : constant Exception_Declaration_Ptr :=
        New_Exception_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  794 =>
--#line  9893

   declare
      New_Node : constant Handled_Statements_Ptr :=
        New_Handled_Statements_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Statements (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Exception_Handlers (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  795 =>
--#line  9906

   declare
      New_Node : constant Handled_Statements_Ptr :=
        New_Handled_Statements_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Statements (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  796 =>
--#line  9921

   declare
      Wrap1 : constant Exception_Handler_Ptr :=
        New_Exception_Handler_Node (The_Context);
      Wrap2 : constant Primary_Choise_Lists.List :=
        Primary_Choise_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Choice_Parameter_Specification (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Primary_Choise_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Handler_Statements (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Exception_Choices (Wrap1.all, T (Wrap2));
   end;


when  797 =>
--#line  9938

   declare
      Wrap1 : constant Exception_Handler_Ptr :=
        New_Exception_Handler_Node (The_Context);
      Wrap2 : constant Primary_Choise_Lists.List :=
        Primary_Choise_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Choice_Parameter_Specification (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Primary_Choise_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_Handler_Statements (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Exception_Choices (Wrap1.all, T (Wrap2));
   end;


when  798 =>
--#line  9955

   declare
      Wrap1 : constant Exception_Handler_Ptr :=
        New_Exception_Handler_Node (The_Context);
      Wrap2 : constant Primary_Choise_Lists.List :=
        Primary_Choise_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Primary_Choise_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Handler_Statements (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Exception_Choices (Wrap1.all, T (Wrap2));
   end;


when  799 =>
--#line  9971

   declare
      Wrap1 : constant Exception_Handler_Ptr :=
        New_Exception_Handler_Node (The_Context);
      Wrap2 : constant Primary_Choise_Lists.List :=
        Primary_Choise_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Primary_Choise_Lists.Add (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_Handler_Statements (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Exception_Choices (Wrap1.all, T (Wrap2));
   end;


when  800 =>
--#line  9990

   declare
      New_Node : constant Choice_Parameter_Specification_Ptr :=
        New_Choice_Parameter_Specification_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  801 =>
--#line  10005
 
yyval := 
yy.value_stack(yy.tos);

when  802 =>
--#line  10007

   declare
      New_Node : constant Others_Choice_Ptr :=
        New_Others_Choice_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  803 =>
--#line  10021

   declare
      New_Node : constant Raise_Statement_Ptr :=
        New_Raise_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  804 =>
--#line  10032

   declare
      New_Node : constant Raise_Statement_Ptr :=
        New_Raise_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Raised_Exception (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Raise_Statement_Message (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  805 =>
--#line  10045

   declare
      New_Node : constant Raise_Statement_Ptr :=
        New_Raise_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Raised_Exception (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  806 =>
--#line  10060
 
yyval := 
yy.value_stack(yy.tos);

when  807 =>
--#line  10062
 
yyval := 
yy.value_stack(yy.tos);

when  808 =>
--#line  10067

   if 
yy.value_stack(yy.tos-1).all in Procedure_Specification_Node then
      declare
         New_Node : constant Generic_Procedure_Declaration_Ptr :=
           New_Generic_Procedure_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
         Set_Generic_Formal_Part (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Generic_Function_Declaration_Ptr :=
           New_Generic_Function_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
         Set_Generic_Formal_Part (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  809 =>
--#line  10096

   declare
      New_Node : constant Generic_Package_Declaration_Ptr :=
        New_Generic_Package_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Generic_Formal_Part (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  810 =>
--#line  10112

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
   end;


when  811 =>
--#line  10122

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  812 =>
--#line  10136
 
yyval := 
yy.value_stack(yy.tos);

when  813 =>
--#line  10138
 
yyval := 
yy.value_stack(yy.tos);

when  814 =>
--#line  10140
 
yyval := 
yy.value_stack(yy.tos);

when  815 =>
--#line  10142
 
yyval := 
yy.value_stack(yy.tos);

when  816 =>
--#line  10147

   declare
      New_Node : constant Package_Instantiation_Ptr :=
        New_Package_Instantiation_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Generic_Unit_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Generic_Actual_Part (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  817 =>
--#line  10161

   declare
      New_Node : constant Package_Instantiation_Ptr :=
        New_Package_Instantiation_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Generic_Unit_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  818 =>
--#line  10174

   if 
yy.value_stack(yy.tos-5).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Instantiation_Ptr :=
           New_Procedure_Instantiation_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Generic_Unit_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_Generic_Actual_Part (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Instantiation_Ptr :=
           New_Function_Instantiation_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Generic_Unit_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_Generic_Actual_Part (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  819 =>
--#line  10204

   if 
yy.value_stack(yy.tos-4).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Instantiation_Ptr :=
           New_Procedure_Instantiation_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Generic_Unit_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Instantiation_Ptr :=
           New_Function_Instantiation_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
         Set_Overriding_Indicator_Kind (New_Node.all, Last_Overriding_Kind);
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Generic_Unit_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  820 =>
--#line  10232

   if 
yy.value_stack(yy.tos-5).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Instantiation_Ptr :=
           New_Procedure_Instantiation_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Generic_Unit_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_Generic_Actual_Part (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Instantiation_Ptr :=
           New_Function_Instantiation_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Generic_Unit_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_Generic_Actual_Part (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  821 =>
--#line  10260

   if 
yy.value_stack(yy.tos-4).all in Procedure_Specification_Node then
      declare
         New_Node : constant Procedure_Instantiation_Ptr :=
           New_Procedure_Instantiation_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Generic_Unit_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Function_Instantiation_Ptr :=
           New_Function_Instantiation_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Generic_Unit_Name (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  822 =>
--#line  10289

   declare
      Wrap1 : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Association_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-2));
   end;


when  823 =>
--#line  10299

   declare
      Wrap1 : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Association_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  824 =>
--#line  10312

   declare
      New_Node : constant Generic_Association_Ptr :=
        New_Generic_Association_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Formal_Parameter (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Actual_Parameter (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  825 =>
--#line  10325

   declare
      New_Node : constant Generic_Association_Ptr :=
        New_Generic_Association_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Actual_Parameter (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  826 =>
--#line  10340
 
yyval := 
yy.value_stack(yy.tos);

when  827 =>
--#line  10345

   declare
      Wrap1 : constant Formal_Object_Declaration_Ptr :=
        New_Formal_Object_Declaration_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-6));
      Set_Mode_Kind (Wrap1.all, Modes.
yy.value_stack(yy.tos-4));
      Set_Has_Null_Exclusion (Wrap1.all, True);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Initialization_Expression (Wrap1.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  828 =>
--#line  10366

   declare
      Wrap1 : constant Formal_Object_Declaration_Ptr :=
        New_Formal_Object_Declaration_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Mode_Kind (Wrap1.all, Modes.
yy.value_stack(yy.tos-3));
      Set_Has_Null_Exclusion (Wrap1.all, True);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  829 =>
--#line  10386

   declare
      Wrap1 : constant Formal_Object_Declaration_Ptr :=
        New_Formal_Object_Declaration_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Mode_Kind (Wrap1.all, Modes.
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Initialization_Expression (Wrap1.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  830 =>
--#line  10406

   declare
      Wrap1 : constant Formal_Object_Declaration_Ptr :=
        New_Formal_Object_Declaration_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Mode_Kind (Wrap1.all, Modes.
yy.value_stack(yy.tos-2));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  831 =>
--#line  10425

   declare
      Wrap1 : constant Formal_Object_Declaration_Ptr :=
        New_Formal_Object_Declaration_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Has_Null_Exclusion (Wrap1.all, True);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Initialization_Expression (Wrap1.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  832 =>
--#line  10445

   declare
      Wrap1 : constant Formal_Object_Declaration_Ptr :=
        New_Formal_Object_Declaration_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Has_Null_Exclusion (Wrap1.all, True);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  833 =>
--#line  10464

   declare
      Wrap1 : constant Formal_Object_Declaration_Ptr :=
        New_Formal_Object_Declaration_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-4));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Initialization_Expression (Wrap1.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  834 =>
--#line  10483

   declare
      Wrap1 : constant Formal_Object_Declaration_Ptr :=
        New_Formal_Object_Declaration_Node (The_Context);
      Wrap2 : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Object_Declaration_Subtype (Wrap1.all, T (Wrap2));
   end;


when  835 =>
--#line  10501

   declare
      New_Node : constant Formal_Object_Declaration_Ptr :=
        New_Formal_Object_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_Mode_Kind (New_Node.all, Modes.
yy.value_stack(yy.tos-3));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  836 =>
--#line  10516

   declare
      New_Node : constant Formal_Object_Declaration_Ptr :=
        New_Formal_Object_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Mode_Kind (New_Node.all, Modes.
yy.value_stack(yy.tos-2));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  837 =>
--#line  10530

   declare
      New_Node : constant Formal_Object_Declaration_Ptr :=
        New_Formal_Object_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Initialization_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  838 =>
--#line  10544

   declare
      New_Node : constant Formal_Object_Declaration_Ptr :=
        New_Formal_Object_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Names (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Object_Declaration_Subtype (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  839 =>
--#line  10560

   declare
      New_Node : constant Formal_Type_Declaration_Ptr :=
        New_Formal_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Discriminant_Part (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  840 =>
--#line  10574

   declare
      New_Node : constant Formal_Type_Declaration_Ptr :=
        New_Formal_Type_Declaration_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Type_Declaration_View (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  841 =>
--#line  10590
 
yyval := 
yy.value_stack(yy.tos);

when  842 =>
--#line  10592
 
yyval := 
yy.value_stack(yy.tos);

when  843 =>
--#line  10594
 
yyval := 
yy.value_stack(yy.tos);

when  844 =>
--#line  10596
 
yyval := 
yy.value_stack(yy.tos);

when  845 =>
--#line  10598
 
yyval := 
yy.value_stack(yy.tos);

when  846 =>
--#line  10600
 
yyval := 
yy.value_stack(yy.tos);

when  847 =>
--#line  10602
 
yyval := 
yy.value_stack(yy.tos);

when  848 =>
--#line  10604
 
yyval := 
yy.value_stack(yy.tos);

when  849 =>
--#line  10606
 
yyval := 
yy.value_stack(yy.tos);

when  850 =>
--#line  10608
 
yyval := 
yy.value_stack(yy.tos);

when  851 =>
--#line  10610
 
yyval := 
yy.value_stack(yy.tos);

when  852 =>
--#line  10615

   declare
      New_Node : constant Formal_Tagged_Private_Type_Ptr :=
        New_Formal_Tagged_Private_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Has_Tagged (New_Node.all, True);
      Set_Trait_Kind (New_Node.all, A_Limited_Trait);
      Set_Has_Limited (New_Node.all, True);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  853 =>
--#line  10632

   declare
      New_Node : constant Formal_Tagged_Private_Type_Ptr :=
        New_Formal_Tagged_Private_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Has_Tagged (New_Node.all, True);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  854 =>
--#line  10647

   declare
      New_Node : constant Formal_Tagged_Private_Type_Ptr :=
        New_Formal_Tagged_Private_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Has_Tagged (New_Node.all, True);
      Set_Trait_Kind (New_Node.all, A_Limited_Trait);
      Set_Has_Limited (New_Node.all, True);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  855 =>
--#line  10662

   declare
      New_Node : constant Formal_Tagged_Private_Type_Ptr :=
        New_Formal_Tagged_Private_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Has_Tagged (New_Node.all, True);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  856 =>
--#line  10675

   declare
      New_Node : constant Formal_Private_Type_Ptr :=
        New_Formal_Private_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Trait_Kind (New_Node.all, A_Limited_Trait);
      Set_Has_Limited (New_Node.all, True);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  857 =>
--#line  10689

   declare
      New_Node : constant Formal_Private_Type_Ptr :=
        New_Formal_Private_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  858 =>
--#line  10704

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Has_Synchronized (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, A_Private_Trait);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  859 =>
--#line  10722

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Has_Synchronized (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, A_Private_Trait);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  860 =>
--#line  10739

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Has_Synchronized (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  861 =>
--#line  10754

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Trait_Kind (New_Node.all, A_Limited_Trait);
      Set_Has_Limited (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, A_Private_Trait);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  862 =>
--#line  10773

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Trait_Kind (New_Node.all, A_Limited_Trait);
      Set_Has_Limited (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, A_Private_Trait);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  863 =>
--#line  10791

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Trait_Kind (New_Node.all, A_Limited_Trait);
      Set_Has_Limited (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  864 =>
--#line  10807

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, A_Private_Trait);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  865 =>
--#line  10824

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, A_Private_Trait);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  866 =>
--#line  10840

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Trait_Kind (New_Node.all, An_Abstract_Trait);
      Set_Has_Abstract (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  867 =>
--#line  10854

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Has_Synchronized (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, A_Private_Trait);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  868 =>
--#line  10870

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Has_Synchronized (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, A_Private_Trait);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  869 =>
--#line  10885

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Has_Synchronized (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  870 =>
--#line  10898

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Trait_Kind (New_Node.all, A_Limited_Trait);
      Set_Has_Limited (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, A_Private_Trait);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  871 =>
--#line  10915

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Trait_Kind (New_Node.all, A_Limited_Trait);
      Set_Has_Limited (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, A_Private_Trait);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  872 =>
--#line  10931

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Trait_Kind (New_Node.all, A_Limited_Trait);
      Set_Has_Limited (New_Node.all, True);
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  873 =>
--#line  10945

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Progenitor_List (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, A_Private_Trait);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  874 =>
--#line  10960

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Trait_Kind (New_Node.all, A_Private_Trait);
      Set_Has_Private (New_Node.all, True);
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  875 =>
--#line  10974

   declare
      New_Node : constant Formal_Derived_Type_Ptr :=
        New_Formal_Derived_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  876 =>
--#line  10989

   declare
      New_Node : constant Formal_Discrete_Type_Ptr :=
        New_Formal_Discrete_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  877 =>
--#line  11003

   declare
      New_Node : constant Formal_Signed_Integer_Type_Ptr :=
        New_Formal_Signed_Integer_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  878 =>
--#line  11017

   declare
      New_Node : constant Formal_Modular_Type_Ptr :=
        New_Formal_Modular_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  879 =>
--#line  11031

   declare
      New_Node : constant Formal_Floating_Point_Ptr :=
        New_Formal_Floating_Point_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  880 =>
--#line  11045

   declare
      New_Node : constant Formal_Ordinary_Fixed_Point_Ptr :=
        New_Formal_Ordinary_Fixed_Point_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  881 =>
--#line  11059

   declare
      New_Node : constant Formal_Decimal_Fixed_Point_Ptr :=
        New_Formal_Decimal_Fixed_Point_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  882 =>
--#line  11073

   if 
yy.value_stack(yy.tos).all in Unconstrained_Array_Node then
      declare
         New_Node : constant Formal_Unconstrained_Array_Ptr :=
           New_Formal_Unconstrained_Array_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
         Set_Array_Definition (New_Node.all, 
yy.value_stack(yy.tos));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Formal_Constrained_Array_Ptr :=
           New_Formal_Constrained_Array_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
         Set_Array_Definition (New_Node.all, 
yy.value_stack(yy.tos));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  883 =>
--#line  11100

   declare
      New_Node : constant Formal_Access_Type_Ptr :=
        New_Formal_Access_Type_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Access_Definition (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  884 =>
--#line  11115
 
yyval := 
yy.value_stack(yy.tos);

when  885 =>
--#line  11120
 
yyval := 
yy.value_stack(yy.tos);

when  886 =>
--#line  11122
 
yyval := 
yy.value_stack(yy.tos);

when  887 =>
--#line  11127

   if 
yy.value_stack(yy.tos-2).all in Procedure_Specification_Node then
      declare
         New_Node : constant Formal_Procedure_Declaration_Ptr :=
           New_Formal_Procedure_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_Formal_Subprogram_Default (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Formal_Function_Declaration_Ptr :=
           New_Formal_Function_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_Formal_Subprogram_Default (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  888 =>
--#line  11153

   if 
yy.value_stack(yy.tos-1).all in Procedure_Specification_Node then
      declare
         New_Node : constant Formal_Procedure_Declaration_Ptr :=
           New_Formal_Procedure_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Formal_Function_Declaration_Ptr :=
           New_Formal_Function_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  889 =>
--#line  11180

   if 
yy.value_stack(yy.tos-4).all in Procedure_Specification_Node then
      declare
         New_Node : constant Formal_Procedure_Declaration_Ptr :=
           New_Formal_Procedure_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Has_Abstract (New_Node.all, True);
         Set_Formal_Subprogram_Default (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Formal_Function_Declaration_Ptr :=
           New_Formal_Function_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-4));
         Set_Has_Abstract (New_Node.all, True);
         Set_Formal_Subprogram_Default (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  890 =>
--#line  11208

   if 
yy.value_stack(yy.tos-3).all in Procedure_Specification_Node then
      declare
         New_Node : constant Formal_Procedure_Declaration_Ptr :=
           New_Formal_Procedure_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Has_Abstract (New_Node.all, True);
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Formal_Function_Declaration_Ptr :=
           New_Formal_Function_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
         Set_Specification (New_Node.all, 
yy.value_stack(yy.tos-3));
         Set_Has_Abstract (New_Node.all, True);
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  891 =>
--#line  11237
 
yyval := 
yy.value_stack(yy.tos);

when  892 =>
--#line  11239

   declare
      New_Node : constant Box_Expression_Ptr :=
        New_Box_Expression_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  893 =>
--#line  11250

   declare
      New_Node : constant Null_Literal_Ptr :=
        New_Null_Literal_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  894 =>
--#line  11264
 
yyval := 
yy.value_stack(yy.tos);

when  895 =>
--#line  11269

   if 
yy.value_stack(yy.tos-1).all in Primary_Association_Lists.List_Node then
      declare
         New_Node : constant Formal_Package_Declaration_Ptr :=
           New_Formal_Package_Declaration_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
         Set_Name (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Generic_Unit_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_Generic_Actual_Part (New_Node.all, 
yy.value_stack(yy.tos-1));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   else
      declare
         New_Node : constant Formal_Package_Declaration_With_Box_Ptr :=
           New_Formal_Package_Declaration_With_Box_Node (The_Context);
      begin
         
yyval := YYSTYPE (New_Node);
         Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
         Set_Name (New_Node.all, 
yy.value_stack(yy.tos-5));
         Set_Generic_Unit_Name (New_Node.all, 
yy.value_stack(yy.tos-2));
         Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      end;
   end if;


when  897 =>
--#line  11300

   declare
      Wrap1 : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Wrap2 : constant Generic_Association_Ptr :=
        New_Generic_Association_Node (The_Context);
      Wrap3 : constant Others_Choice_Ptr :=
        New_Others_Choice_Node (The_Context);
      Wrap4 : constant Box_Expression_Ptr :=
        New_Box_Expression_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos-3).all));
      Set_Start_Position (Wrap4.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap4.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Actual_Parameter (Wrap2.all, T (Wrap4));
      Set_End_Position (Wrap2.all, End_Position (T (Wrap4).all));
      Set_Start_Position (Wrap2.all, Start_Position (T (Wrap3).all));
      Set_Formal_Parameter (Wrap2.all, T (Wrap3));
      Primary_Association_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  898 =>
--#line  11324

   declare
      Wrap1 : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
   end;


when  899 =>
--#line  11333

   declare
      Wrap1 : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Association_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-3));
      Primary_Association_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  900 =>
--#line  11344

   declare
      Wrap1 : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Association_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-2));
   end;


when  901 =>
--#line  11354

   declare
      Wrap1 : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Association_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-2));
      Primary_Association_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  902 =>
--#line  11365

   declare
      Wrap1 : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Association_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  903 =>
--#line  11378
 
yyval := 
yy.value_stack(yy.tos);

when  904 =>
--#line  11380

   declare
      Wrap1 : constant Generic_Association_Ptr :=
        New_Generic_Association_Node (The_Context);
      Wrap2 : constant Box_Expression_Ptr :=
        New_Box_Expression_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Formal_Parameter (Wrap1.all, 
yy.value_stack(yy.tos-2));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Actual_Parameter (Wrap1.all, T (Wrap2));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap2).all));
   end;


when  905 =>
--#line  11401
 
yyval := 
yy.value_stack(yy.tos);

when  906 =>
--#line  11403
 
yyval := 
yy.value_stack(yy.tos);

when  907 =>
--#line  11405
 
yyval := 
yy.value_stack(yy.tos);

when  908 =>
--#line  11410
 
yyval := 
yy.value_stack(yy.tos);

when  909 =>
--#line  11412

   declare
      New_Node : constant Attribute_Reference_Ptr :=
        New_Attribute_Reference_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Prefix (New_Node.all, 
yy.value_stack(yy.tos-2));
      Set_Attribute_Designator_Identifier (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  910 =>
--#line  11428

   declare
      New_Node : constant At_Clause_Ptr :=
        New_At_Clause_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-5).all));
      Set_Representation_Clause_Name (New_Node.all, 
yy.value_stack(yy.tos-4));
      Set_Representation_Clause_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  911 =>
--#line  11441

   declare
      New_Node : constant Attribute_Definition_Clause_Ptr :=
        New_Attribute_Definition_Clause_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Representation_Clause_Name (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_Representation_Clause_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  912 =>
--#line  11457
 
yyval := 
yy.value_stack(yy.tos-3);

when  913 =>
--#line  11462
 
yyval := 
yy.value_stack(yy.tos);

when  914 =>
--#line  11467

   declare
      New_Node : constant Record_Representation_Clause_Ptr :=
        New_Record_Representation_Clause_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Representation_Clause_Name (New_Node.all, 
yy.value_stack(yy.tos-6));
      Set_Component_Clauses (New_Node.all, 
yy.value_stack(yy.tos-3));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  915 =>
--#line  11480

   declare
      New_Node : constant Record_Representation_Clause_Ptr :=
        New_Record_Representation_Clause_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-6).all));
      Set_Representation_Clause_Name (New_Node.all, 
yy.value_stack(yy.tos-5));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  916 =>
--#line  11495

   declare
      Wrap1 : constant Component_Clause_Ptr :=
        New_Component_Clause_Node (The_Context);
      Wrap2 : constant Discrete_Simple_Expression_Range_Ptr :=
        New_Discrete_Simple_Expression_Range_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-7).all));
      Set_Representation_Clause_Name (Wrap1.all, 
yy.value_stack(yy.tos-7));
      Set_Component_Clause_Position (Wrap1.all, 
yy.value_stack(yy.tos-5));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-4).all));
      Set_Lower_Bound (Wrap2.all, 
yy.value_stack(yy.tos-3));
      Set_Upper_Bound (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Component_Clause_Range (Wrap1.all, T (Wrap2));
   end;


when  917 =>
--#line  11515
 
yyval := 
yy.value_stack(yy.tos);

when  918 =>
--#line  11517
 
yyval := 
yy.value_stack(yy.tos);

when  919 =>
--#line  11522
 
yyval := 
yy.value_stack(yy.tos);

when  920 =>
--#line  11527
 
yyval := 
yy.value_stack(yy.tos);

when  921 =>
--#line  11532
 
yyval := 
yy.value_stack(yy.tos);

when  922 =>
--#line  11537

   declare
      New_Node : constant Code_Statement_Ptr :=
        New_Code_Statement_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Qualified_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  923 =>
--#line  11552
 
yyval := 
yy.value_stack(yy.tos);

when  924 =>
--#line  11554
 
yyval := 
yy.value_stack(yy.tos-2);

when  925 =>
--#line  11559
 
yyval := 
yy.value_stack(yy.tos);

when  926 =>
--#line  11561
 
yyval := 
yy.value_stack(yy.tos);

when  927 =>
--#line  11566

   declare
      New_Node : constant Delta_Constraint_Ptr :=
        New_Delta_Constraint_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Delta_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Real_Range_Constraint (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  928 =>
--#line  11579

   declare
      New_Node : constant Delta_Constraint_Ptr :=
        New_Delta_Constraint_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Delta_Expression (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  929 =>
--#line  11594
 
yyval := 
yy.value_stack(yy.tos-4);

when  930 =>
--#line  11599

   declare
      New_Node : constant Record_Representation_Clause_Ptr :=
        New_Record_Representation_Clause_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Mod_Clause_Expression (New_Node.all, 
yy.value_stack(yy.tos-1));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  931 =>
--#line  11614

   declare
      Wrap1 : constant Variant_Part_Ptr :=
        Variant_Part_Ptr (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_End_Pragmas (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  932 =>
--#line  11625

   declare
      Wrap1 : constant Variant_Part_Ptr :=
        Variant_Part_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
   end;


when  933 =>
--#line  11637

   declare
      Wrap1 : constant Primary_Identifier_Lists.List :=
        Primary_Identifier_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Identifier_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  934 =>
--#line  11647

   declare
      Wrap1 : constant Primary_Identifier_Lists.List :=
        Primary_Identifier_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Identifier_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos));
   end;


when  935 =>
--#line  11660
 
yyval := 
yy.value_stack(yy.tos);

when  936 =>
--#line  11662
 
yyval := 
yy.value_stack(yy.tos);

when  937 =>
--#line  11664
 
yyval := 
yy.value_stack(yy.tos);

when  938 =>
--#line  11669
 
yyval := 
yy.value_stack(yy.tos);

when  939 =>
--#line  11671
 
yyval := 
yy.value_stack(yy.tos);

when  940 =>
--#line  11676

   declare
      New_Node : constant Subtype_Indication_Ptr :=
        New_Subtype_Indication_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Subtype_Mark (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  941 =>
--#line  11691

   declare
      Wrap1 : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Association_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-2));
   end;


when  942 =>
--#line  11701

   declare
      Wrap1 : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Primary_Association_Lists.Add (Wrap1.all, 
yy.value_stack(yy.tos-1));
   end;


when  943 =>
--#line  11714

   declare
      New_Node : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Association_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  944 =>
--#line  11724

   declare
      New_Node : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Association_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  945 =>
--#line  11737
 
yyval := 
yy.value_stack(yy.tos-1);

when  946 =>
--#line  11742
 
yyval := 
yy.value_stack(yy.tos);

when  947 =>
--#line  11747

   declare
      New_Node : constant Primary_Defining_Name_Lists.List :=
        Primary_Defining_Name_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Defining_Name_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  948 =>
--#line  11757

   declare
      New_Node : constant Primary_Defining_Name_Lists.List :=
        Primary_Defining_Name_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Defining_Name_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  949 =>
--#line  11770
 
yyval := 
yy.value_stack(yy.tos);

when  950 =>
--#line  11775

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  951 =>
--#line  11785

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  952 =>
--#line  11798

   declare
      New_Node : constant Primary_Identifier_Lists.List :=
        Primary_Identifier_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Identifier_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  953 =>
--#line  11808

   declare
      New_Node : constant Primary_Identifier_Lists.List :=
        Primary_Identifier_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Identifier_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  954 =>
--#line  11821

   declare
      New_Node : constant Primary_Definition_Lists.List :=
        Primary_Definition_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Definition_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  955 =>
--#line  11831

   declare
      New_Node : constant Primary_Definition_Lists.List :=
        Primary_Definition_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Definition_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  956 =>
--#line  11844

   declare
      New_Node : constant Primary_Definition_Lists.List :=
        Primary_Definition_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Definition_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  957 =>
--#line  11854

   declare
      New_Node : constant Primary_Definition_Lists.List :=
        Primary_Definition_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Definition_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  958 =>
--#line  11867

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  959 =>
--#line  11877

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  960 =>
--#line  11890
 
yyval := 
yy.value_stack(yy.tos);

when  961 =>
--#line  11895
 
yyval := 
yy.value_stack(yy.tos);

when  962 =>
--#line  11897
 
yyval := 
yy.value_stack(yy.tos);

when  965 =>
--#line  11907
 
yyval := 
yy.value_stack(yy.tos);

when  966 =>
--#line  11909
 
yyval := 
yy.value_stack(yy.tos);

when  967 =>
--#line  11914

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  968 =>
--#line  11924

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  969 =>
--#line  11937

   declare
      New_Node : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Statement_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  970 =>
--#line  11947

   declare
      New_Node : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Statement_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  971 =>
--#line  11960

   declare
      New_Node : constant Primary_Variant_Lists.List :=
        Primary_Variant_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Variant_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  972 =>
--#line  11970

   declare
      New_Node : constant Primary_Variant_Lists.List :=
        Primary_Variant_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Variant_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  973 =>
--#line  11983

   declare
      New_Node : constant Primary_Choise_Lists.List :=
        Primary_Choise_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Choise_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  974 =>
--#line  11993

   declare
      New_Node : constant Primary_Choise_Lists.List :=
        Primary_Choise_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Choise_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  975 =>
--#line  12006

   declare
      New_Node : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Primary_Expression_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  976 =>
--#line  12017

   declare
      New_Node : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Primary_Expression_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  977 =>
--#line  12031

   declare
      Wrap1 : constant Tagged_Incomplete_Type_Definition_Ptr :=
        New_Tagged_Incomplete_Type_Definition_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_Has_Tagged (Wrap1.all, True);
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  978 =>
--#line  12046

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  979 =>
--#line  12056

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  980 =>
--#line  12069
 
yyval := 
yy.value_stack(yy.tos);

when  981 =>
--#line  12071
 
yyval := 
yy.value_stack(yy.tos);

when  982 =>
--#line  12076
 
yyval := 
yy.value_stack(yy.tos-1);

when  983 =>
--#line  12081

   declare
      New_Node : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Association_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  984 =>
--#line  12091

   declare
      New_Node : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Association_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  985 =>
--#line  12104
 
yyval := 
yy.value_stack(yy.tos-1);

when  986 =>
--#line  12109
 
yyval := 
yy.value_stack(yy.tos);

when  987 =>
--#line  12111
 
yyval := 
yy.value_stack(yy.tos);

when  988 =>
--#line  12116

   declare
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Call_Node : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node1.all, 
yy.value_stack(yy.tos-2));
      Set_Start_Position (Arg_Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Arg_Node1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-1).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Prefix (Call_Node.all, T (Sym_Node));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  989 =>
--#line  12149

   declare
      Call_Node  : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node   : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
      Arg_2_Node : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
   begin
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_2_Node));
      Set_Actual_Parameter (Arg_Node.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node.all, T (Sym_Node));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-1).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  990 =>
--#line  12184

   declare
      Node1 : constant And_Then_Short_Circuit_Ptr:=
        New_And_Then_Short_Circuit_Node (The_Context);
   begin
      Set_Short_Circuit_Operation_Left_Expression (Node1.all, 
yy.value_stack(yy.tos-3));
      Set_Short_Circuit_Operation_Right_Expression (Node1.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Node1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_End_Position (Node1.all, End_Position (
yy.value_stack(yy.tos).all));
      
yyval := YYSTYPE (Node1);
   end;


when  991 =>
--#line  12197

   declare
      Node1 : constant And_Then_Short_Circuit_Ptr:=
        New_And_Then_Short_Circuit_Node (The_Context);
   begin
      Set_Short_Circuit_Operation_Right_Expression (Node1.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Node1.all, End_Position (
yy.value_stack(yy.tos).all));
      
yyval := YYSTYPE (Node1);
   end;


when  992 =>
--#line  12212

   declare
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Call_Node : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node1.all, 
yy.value_stack(yy.tos-2));
      Set_Start_Position (Arg_Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Arg_Node1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-1).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Prefix (Call_Node.all, T (Sym_Node));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  993 =>
--#line  12245

   declare
      Call_Node  : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node   : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
      Arg_2_Node : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
   begin
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_2_Node));
      Set_Actual_Parameter (Arg_Node.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node.all, T (Sym_Node));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-1).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  994 =>
--#line  12280

   declare
      Node1 : constant Or_Else_Short_Circuit_Ptr:=
        New_Or_Else_Short_Circuit_Node (The_Context);
   begin
      Set_Short_Circuit_Operation_Left_Expression (Node1.all, 
yy.value_stack(yy.tos-3));
      Set_Short_Circuit_Operation_Right_Expression (Node1.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Node1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_End_Position (Node1.all, End_Position (
yy.value_stack(yy.tos).all));
      
yyval := YYSTYPE (Node1);
   end;


when  995 =>
--#line  12293

   declare
      Node1 : constant Or_Else_Short_Circuit_Ptr:=
        New_Or_Else_Short_Circuit_Node (The_Context);
   begin
      Set_Short_Circuit_Operation_Right_Expression (Node1.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Node1.all, End_Position (
yy.value_stack(yy.tos).all));
      
yyval := YYSTYPE (Node1);
   end;


when  996 =>
--#line  12308

   declare
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Call_Node : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node1.all, 
yy.value_stack(yy.tos-2));
      Set_Start_Position (Arg_Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Arg_Node1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-1).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Prefix (Call_Node.all, T (Sym_Node));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  997 =>
--#line  12341

   declare
      Call_Node  : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node   : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
      Arg_2_Node : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
   begin
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_2_Node));
      Set_Actual_Parameter (Arg_Node.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node.all, T (Sym_Node));
      Set_Name_Image (Sym_Node.all, '"' & To_String (Raw_Image (
yy.value_stack(yy.tos-1).all)) & '"');
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  998 =>
--#line  12376

   declare
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Call_Node : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  999 =>
--#line  12404

   declare
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Call_Node : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node1.all, 
yy.value_stack(yy.tos-2));
      Set_Start_Position (Arg_Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Arg_Node1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  1000 =>
--#line  12432

   declare
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Call_Node : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  1001 =>
--#line  12460

   declare
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Call_Node : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node1.all, 
yy.value_stack(yy.tos-2));
      Set_Start_Position (Arg_Node1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Arg_Node1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  1002 =>
--#line  12488

   declare
      Arg_Node1  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_Node2  : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Call_Node : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
   begin
      Set_Actual_Parameter (Arg_Node2.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node2.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node1));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node2));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node.all, 
yy.value_stack(yy.tos-1));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  1003 =>
--#line  12516

   declare
      Call_Node  : constant Function_Call_Ptr :=
        New_Function_Call_Node (The_Context);
      Arg_List   : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
      Arg_Node   : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
      Sym_Node   : constant Operator_Symbol_Ptr :=
        New_Operator_Symbol_Node (The_Context);
      Arg_2_Node : constant Parameter_Association_Ptr :=
        New_Parameter_Association_Node (The_Context);
   begin
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_2_Node));
      Set_Actual_Parameter (Arg_Node.all, 
yy.value_stack(yy.tos));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Association_Lists.Add (Arg_List.all, T (Arg_Node));
      Set_Function_Call_Parameters (Call_Node.all, T (Arg_List));
      Set_Start_Position (Arg_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Arg_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Prefix (Call_Node.all, T (Sym_Node));
      Set_Name_Image (Sym_Node.all, """**""");
      Set_Start_Position (Sym_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Sym_Node.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Call_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Call_Node.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Is_Prefix_Call (Call_Node.all, False);
      
yyval := YYSTYPE (Call_Node);
   end;


when  1004 =>
--#line  12551

   declare
      New_Node : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Statement_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1005 =>
--#line  12561

   declare
      New_Node : constant Primary_Statement_Lists.List :=
        Primary_Statement_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Statement_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1006 =>
--#line  12574

   declare
      New_Node : constant Primary_Defining_Name_Lists.List :=
        Primary_Defining_Name_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Defining_Name_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1007 =>
--#line  12584

   declare
      New_Node : constant Primary_Defining_Name_Lists.List :=
        Primary_Defining_Name_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Defining_Name_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1008 =>
--#line  12597

   declare
      Wrap1 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-4));
      Wrap2 : constant Elsif_Path_Ptr :=
        New_Elsif_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Condition_Expression (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Sequence_Of_Statements (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1009 =>
--#line  12615

   declare
      Wrap1 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap2 : constant Elsif_Path_Ptr :=
        New_Elsif_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Condition_Expression (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Sequence_Of_Statements (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1010 =>
--#line  12636

   declare
      New_Node : constant Else_Path_Ptr :=
        New_Else_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position (New_Node.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Sequence_Of_Statements (New_Node.all, 
yy.value_stack(yy.tos));
      Set_End_Position (New_Node.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  1011 =>
--#line  12651

   declare
      New_Node : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Path_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1012 =>
--#line  12661

   declare
      New_Node : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Path_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1013 =>
--#line  12674
 
yyval := 
yy.value_stack(yy.tos-1);

when  1014 =>
--#line  12679

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
   end;


when  1015 =>
--#line  12692
 
yyval := 
yy.value_stack(yy.tos);

when  1016 =>
--#line  12697

   declare
      New_Node : constant Primary_Parameter_Lists.List :=
        Primary_Parameter_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Parameter_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1017 =>
--#line  12707

   declare
      New_Node : constant Primary_Parameter_Lists.List :=
        Primary_Parameter_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Parameter_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1018 =>
--#line  12720
 
yyval := 
yy.value_stack(yy.tos);

when  1019 =>
--#line  12722
 
yyval := 
yy.value_stack(yy.tos);

when  1020 =>
--#line  12727
 
yyval := 
yy.value_stack(yy.tos-1);

when  1021 =>
--#line  12732
 
yyval := 
yy.value_stack(yy.tos-2);

when  1022 =>
--#line  12737

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1023 =>
--#line  12747

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1024 =>
--#line  12760

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
   end;


when  1025 =>
--#line  12770

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  1026 =>
--#line  12784
 
yyval := 
yy.value_stack(yy.tos);

when  1027 =>
--#line  12789

   declare
      New_Node : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Expression_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1028 =>
--#line  12799

   declare
      New_Node : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Expression_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1029 =>
--#line  12812

   declare
      New_Node : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Expression_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1030 =>
--#line  12822

   declare
      New_Node : constant Primary_Expression_Lists.List :=
        Primary_Expression_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Expression_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1031 =>
--#line  12835

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1032 =>
--#line  12845

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1033 =>
--#line  12858

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
   end;


when  1034 =>
--#line  12868

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  1035 =>
--#line  12882
 
yyval := 
yy.value_stack(yy.tos-1);

when  1036 =>
--#line  12887

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1037 =>
--#line  12897

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1038 =>
--#line  12910

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
   end;


when  1039 =>
--#line  12920

   declare
      Wrap1 : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  1040 =>
--#line  12934

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1041 =>
--#line  12944

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1042 =>
--#line  12957

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1043 =>
--#line  12967

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1044 =>
--#line  12980
 
yyval := 
yy.value_stack(yy.tos-1);

when  1045 =>
--#line  12985
 
yyval := 
yy.value_stack(yy.tos-1);

when  1046 =>
--#line  12990

   declare
      Wrap1 : constant Handled_Statements_Ptr :=
        Handled_Statements_Ptr (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Identifier (Wrap1.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  1047 =>
--#line  13002

   declare
      Wrap1 : constant Handled_Statements_Ptr :=
        Handled_Statements_Ptr (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos).all));
   end;


when  1048 =>
--#line  13016
 
yyval := 
yy.value_stack(yy.tos-1);

when  1050 =>
--#line  13025

   declare
      Wrap1 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-4));
      Wrap2 : constant Or_Path_Ptr :=
        New_Or_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Guard (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_Pragmas (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_Sequence_Of_Statements (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1051 =>
--#line  13042

   declare
      Wrap1 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-3));
      Wrap2 : constant Or_Path_Ptr :=
        New_Or_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Guard (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_Sequence_Of_Statements (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1052 =>
--#line  13058

   declare
      Wrap1 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-3));
      Wrap2 : constant Or_Path_Ptr :=
        New_Or_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Pragmas (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_Sequence_Of_Statements (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1053 =>
--#line  13074

   declare
      Wrap1 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.List (
yy.value_stack(yy.tos-2));
      Wrap2 : constant Or_Path_Ptr :=
        New_Or_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Sequence_Of_Statements (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1054 =>
--#line  13089

   declare
      Wrap1 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap2 : constant Or_Path_Ptr :=
        New_Or_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Guard (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Set_Pragmas (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_Sequence_Of_Statements (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1055 =>
--#line  13106

   declare
      Wrap1 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap2 : constant Or_Path_Ptr :=
        New_Or_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Guard (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_Sequence_Of_Statements (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1056 =>
--#line  13122

   declare
      Wrap1 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap2 : constant Or_Path_Ptr :=
        New_Or_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Pragmas (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Set_Sequence_Of_Statements (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1057 =>
--#line  13138

   declare
      Wrap1 : constant Primary_Path_Lists.List :=
        Primary_Path_Lists.New_List (The_Context);
      Wrap2 : constant Or_Path_Ptr :=
        New_Or_Path_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Sequence_Of_Statements (Wrap2.all, 
yy.value_stack(yy.tos));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos).all));
      Primary_Path_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1058 =>
--#line  13156

   declare
      New_Node : constant Primary_Unit_Lists.List :=
        Primary_Unit_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Unit_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1059 =>
--#line  13166

   declare
      New_Node : constant Primary_Unit_Lists.List :=
        Primary_Unit_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Unit_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1060 =>
--#line  13179

   declare
      New_Node : constant Primary_Clause_Lists.List :=
        Primary_Clause_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Clause_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1061 =>
--#line  13189

   declare
      New_Node : constant Primary_Clause_Lists.List :=
        Primary_Clause_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Clause_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1062 =>
--#line  13202

   declare
      Wrap1 : constant Primary_Handler_Lists.List :=
        Primary_Handler_Lists.List (
yy.value_stack(yy.tos));
      Wrap2 : constant Exception_Handler_Ptr :=
        Exception_Handler_Ptr (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_Pragmas (Wrap2.all, 
yy.value_stack(yy.tos-2));
      Primary_Handler_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1063 =>
--#line  13217

   declare
      Wrap1 : constant Primary_Handler_Lists.List :=
        Primary_Handler_Lists.New_List (The_Context);
      Wrap2 : constant Exception_Handler_Ptr :=
        Exception_Handler_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_Pragmas (Wrap2.all, 
yy.value_stack(yy.tos-1));
      Primary_Handler_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1064 =>
--#line  13233

   declare
      Wrap1 : constant Primary_Handler_Lists.List :=
        Primary_Handler_Lists.List (
yy.value_stack(yy.tos));
      Wrap2 : constant Exception_Handler_Ptr :=
        Exception_Handler_Ptr (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Primary_Handler_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1065 =>
--#line  13246

   declare
      Wrap1 : constant Primary_Handler_Lists.List :=
        Primary_Handler_Lists.New_List (The_Context);
      Wrap2 : constant Exception_Handler_Ptr :=
        Exception_Handler_Ptr (
yy.value_stack(yy.tos));
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-1).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Primary_Handler_Lists.Add (Wrap1.all, T (Wrap2));
   end;


when  1066 =>
--#line  13263

   declare
      New_Node : constant Primary_Handler_Lists.List :=
        Primary_Handler_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Handler_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1067 =>
--#line  13273

   declare
      New_Node : constant Primary_Handler_Lists.List :=
        Primary_Handler_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Handler_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1068 =>
--#line  13286
 
yyval := 
yy.value_stack(yy.tos-1);

when  1069 =>
--#line  13291

   declare
      New_Node : constant Primary_Choise_Lists.List :=
        Primary_Choise_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Choise_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1070 =>
--#line  13301

   declare
      New_Node : constant Primary_Choise_Lists.List :=
        Primary_Choise_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Choise_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1071 =>
--#line  13314
 
yyval := 
yy.value_stack(yy.tos);

when  1072 =>
--#line  13319

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1073 =>
--#line  13329

   declare
      New_Node : constant Primary_Declaration_Lists.List :=
        Primary_Declaration_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Declaration_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1074 =>
--#line  13342

   declare
      New_Node : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Association_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1075 =>
--#line  13352

   declare
      New_Node : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Association_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1076 =>
--#line  13365
 
yyval := 
yy.value_stack(yy.tos);

when  1077 =>
--#line  13370

   declare
      New_Node : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Association_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1078 =>
--#line  13380

   declare
      New_Node : constant Primary_Association_Lists.List :=
        Primary_Association_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Association_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1079 =>
--#line  13393

   declare
      Wrap1 : constant Generic_Association_Ptr :=
        New_Generic_Association_Node (The_Context);
      Wrap2 : constant Others_Choice_Ptr :=
        New_Others_Choice_Node (The_Context);
      Wrap3 : constant Box_Expression_Ptr :=
        New_Box_Expression_Node (The_Context);
   begin
      
yyval := YYSTYPE (Wrap1);
      Set_Start_Position (Wrap1.all, Start_Position (
yy.value_stack(yy.tos-3).all));
      Set_Start_Position (Wrap2.all, Start_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap2.all, End_Position (
yy.value_stack(yy.tos-2).all));
      Set_End_Position (Wrap1.all, End_Position (
yy.value_stack(yy.tos-1).all));
      Set_Start_Position (Wrap3.all, Start_Position (
yy.value_stack(yy.tos).all));
      Set_End_Position (Wrap3.all, End_Position (
yy.value_stack(yy.tos).all));
      Set_Actual_Parameter (Wrap1.all, T (Wrap3));
      Set_End_Position (Wrap1.all, End_Position (T (Wrap3).all));
      Set_Start_Position (Wrap1.all, Start_Position (T (Wrap2).all));
      Set_Formal_Parameter (Wrap1.all, T (Wrap2));
   end;


when  1080 =>
--#line  13419

   declare
      New_Node : constant Primary_Clause_Lists.List :=
        Primary_Clause_Lists.List (
yy.value_stack(yy.tos-1));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Clause_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1081 =>
--#line  13429

   declare
      New_Node : constant Primary_Clause_Lists.List :=
        Primary_Clause_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Clause_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1082 =>
--#line  13442

   declare
      New_Node : constant Primary_Identifier_Lists.List :=
        Primary_Identifier_Lists.List (
yy.value_stack(yy.tos-2));
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Identifier_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1083 =>
--#line  13452

   declare
      New_Node : constant Primary_Identifier_Lists.List :=
        Primary_Identifier_Lists.New_List (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Primary_Identifier_Lists.Add (New_Node.all, 
yy.value_stack(yy.tos));
   end;


when  1084 =>
--#line  13476

   declare
      New_Node : constant Pragma_Ptr := New_Pragma_Node (The_Context);
   begin
      
yyval := YYSTYPE (New_Node);
      Set_Start_Position
        (New_Node.all, (Get_Current_Line, Get_Current_Column - 1));
      Set_Pragma_Name_Image (New_Node.all, "Syntax_Error");
      Set_Pragma_Kind (New_Node.all, Asis.An_Implementation_Defined_Pragma);
      Set_End_Position
        (New_Node.all, (Get_Current_Line, Get_Current_Column - 1));
   end;


when  1085 =>
--#line  13492
 Modes.
yy.value_stack(yy.tos) := An_In_Mode; 

when  1086 =>
--#line  13493
 Modes.
yy.value_stack(yy.tos-1) := An_In_Out_Mode; 

when  1087 =>
--#line  13494
 Modes.
yy.value_stack(yy.tos) := An_Out_Mode; 

when  1088 =>
--#line  13498
 Last_Access_Kind := An_Access_To_Variable; 

when  1089 =>
--#line  13499
 Last_Access_Kind := An_Access_To_Constant; 

when  1090 =>
--#line  13503
 Last_Overriding_Kind := An_Indicator_of_Not_Overriding; 

when  1091 =>
--#line  13504
 Last_Overriding_Kind := An_Indicator_of_Overriding; 

                    when others => null;
                end case;


            -- Pop RHS states and goto next state
            yy.tos      := yy.tos - rule_length(yy.rule_id) + 1;
            if yy.tos > yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.state_stack(yy.tos) := goto_state(yy.state_stack(yy.tos-1) ,
                                 get_lhs_rule(yy.rule_id));

              yy.value_stack(yy.tos) := yyval;

            if yy.debug then
                reduce_debug(yy.rule_id,
                    goto_state(yy.state_stack(yy.tos - 1),
                               get_lhs_rule(yy.rule_id)));
            end if;

        end if;


    end loop;


end yyparse;

   begin
      Scanners.Initialize (Scanner, Source_Buffers.Buffer_Start (Input));
      Line.From := Source_Buffers.Buffer_Start (Input);
      YYParse;
      declare
         From    : Source_Buffers.Cursor;
         To      : Source_Buffers.Cursor;
      begin
         Scanners.Token_Span (Scanner, From, To);
         Line.To := From;
         Lines.Vectors.Add (Line_List, Line);
      end;
      Result := Last_Compilation;
   end Run;

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
end Asis.Gela.Parser;
