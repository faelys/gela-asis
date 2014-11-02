package Asis.Gela.Parser.Tokens is


   subtype YYSTYPE is Asis.Element;
   subtype T       is YYSTYPE;

    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, New_Line_Token, Separator_Token,
         Comment_Token, Identifier_Token, Integer_Literal_Token,
         Real_Literal_Token, Character_Literal_Token, String_Literal_Token,
         Double_Star_Token, Right_Label_Token, Greater_Or_Equal_Token,
         Box_Token, Left_Label_Token, Less_Or_Equal_Token,
         Inequality_Token, Assignment_Token, Arrow_Token,
         Double_Dot_Token, Ampersand_Token, Greater_Token,
         Less_Token, Apostrophe_Token, Left_Parenthesis_Token,
         Right_Parenthesis_Token, Star_Token, Plus_Token,
         Comma_Token, Hyphen_Token, Dot_Token,
         Slash_Token, Colon_Token, Semicolon_Token,
         Equal_Token, Vertical_Line_Token, Abort_Token,
         Abs_Token, Abstract_Token, Accept_Token,
         Access_Token, Aliased_Token, All_Token,
         And_Token, Array_Token, At_Token,
         Begin_Token, Body_Token, Case_Token,
         Constant_Token, Declare_Token, Delay_Token,
         Delta_Token, Digits_Token, Do_Token,
         Else_Token, Elsif_Token, End_Token,
         Entry_Token, Exception_Token, Exit_Token,
         For_Token, Function_Token, Generic_Token,
         Goto_Token, If_Token, In_Token,
         Interface_Token, Is_Token, Limited_Token,
         Loop_Token, Mod_Token, New_Token,
         Not_Token, Null_Token, Of_Token,
         Or_Token, Others_Token, Out_Token,
         Overriding_Token, Package_Token, Pragma_Token,
         Private_Token, Procedure_Token, Protected_Token,
         Raise_Token, Range_Token, Record_Token,
         Rem_Token, Renames_Token, Requeue_Token,
         Return_Token, Reverse_Token, Select_Token,
         Separate_Token, Subtype_Token, Synchronized_Token,
         Tagged_Token, Task_Token, Terminate_Token,
         Then_Token, Type_Token, Until_Token,
         Use_Token, When_Token, While_Token,
         With_Token, Xor_Token );

    Syntax_Error : exception;

end Asis.Gela.Parser.Tokens;
