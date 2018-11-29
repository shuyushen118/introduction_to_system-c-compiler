//===============================
//== xx.c
//==
//== version 4
//== new:
//==      function declarations and calls.
//==      one function arg; local name is <name>ARG .
//===============================

#include "xx-utils.h"

void Program( void );
void Block( void );
void Statement( void );
void Return( void );
void Asm( void );
void If( void );
void While( void );
void Break( void );
void Continue( void );
void Assign( char * );
void Expression( void );
void Term( void );
void Variable( void );
void NumConst( void );
void StrConst( void );
void Ident( void );
void FunCall( char * );
void VarRef( char * );
void GlobalDecls( void );
void GlobalVarDecl( char * , char * );
void FuncDecl( char * , char * );
void Header( void );
void Postamble( void );
void Do( void );

//-----<Do>            ::= “do” <block> “while” ‘(‘ <expression> ‘)’

//----  <header>        ::= 'HEADER' <verbatim>
//----  <program>       ::= <globalDecls> 'main ( )' <block>
//----  <block>         ::= '{' <statement> [ ';' <statement> ]* '}'
//----  <statement>     ::= <asm> | <if> | <while> | <break> | <continue> | <assign>
//----  <return>        ::= "return" '(' <expression> ')'
//----  <asm>           ::= "asm" '{' <verbatim> '}'
//----  <if>            ::= "if" '(' <expression> ')' <block> "else" <block>
//----  <while>         ::= "while" '(' <expression> ')' <block>
//----  <break>         ::= "break"
//----  <continue>      ::= "continue"
//----  <assign>        ::= <name>  '='  <expression>
//----  <expression>    ::= <term>    [ <op> <term>]*
//----- <term>          ::= <numconst> | <strconst> | '(' <expression> ')'
//-----                                | '*' <var>  | '&' <var> | <ident>
//----  <strconst>      ::= '"' [ <char> ]* '"'
//----  <funcall>       ::= <name> '(' <expression> ')'
//----  <varRef>        ::= <name> | '*' <name> | '&' <name>
//----  <globalDecls>   ::= ( <globalVarDecl> | <functionDecl>  |  <asm> )
//----                      [ ';' ( <globalVarDecl> | <functionDecl>  |  <asm> ) ]*
//----  <globalVarDecl> ::= <type> <name>
//----  <funcDecl>      ::= <type> <name> '(' <type> <name> ')' <block>


//---------------------------------
void Do( void )
{
  //save and find the beigining and ending of the do block
  char start_do[80];
  char cease_do[80];
  //get the while
  char* identifier;

  //write the label into string buffer
  sprintf( start_do, "_start_do_%s" , nextLabel() );
  sprintf( cease_do, "_cease_do_%s" , nextLabel() );

  //store the labels
  pushLoopContext( "do", start_do, cease_do );

  printf( "__TEXT__                           \n");
  printf( "                   ;;-- DO         \n");

  printf("__TEXT__            \n" );
  printf("%s:             \n", start_do );

  //perform the do block
  Block();
  //get the while identifier
  identifier = GetName();

  //check for while condition
  if ( strcmp(identifier, "while")!=0 ) Abort("no while condition");

  //perform while
  Match( '(' );

  Expression();

  printf("__TEXT__            \n"  );
  printf("BRzp %s              \n" , cease_do );

  printf("BRnzp %s            \n" , start_do );

  Match( ')' );

  printf("__TEXT__            \n"  );
  // printf("BRnzp %s            \n" , start_do );
  printf("%s:  .BLKW 1               \n" , cease_do );

  popLoopContext();

  // char start_do[80];
  // char cease_do[80];
  //
  // sprintf( start_do, "_start_do_%s" , nextLabel() );
  // sprintf( cease_do, "_cease_do_%s" , nextLabel() );
  //
  // pushLoopContext( "while", start_do, cease_do );
  //
  // printf( "__TEXT__                          \n");
  // printf( "                  ;;-- Do      \n");           //-- "while"
  //
  // //perform the content in the do block
  // Block();
  //
  // Match( '(' );
  //
  // printf("__TEXT__            \n"  );
  // printf("%s:                 \n" , start_do );
  //
  // Expression();
  //
  // printf("__TEXT__            \n"  );
  // printf("BRzp %s              \n" , cease_do );
  //
  // Match( ')' );
  //
  // popLoopContext();


}

void Program( void )
//----- <program>     ::= <globalDecls> 'main ( )' <block>
{
   char name[80];

   GlobalDecls();

   strcpy(name,  GetName());

   if ( 0 != strcmp(name, "main" ) ) Abort(" 'main ( )' not found" );

   Match( '(' );
   Match( ')' );

   printf( "__TEXT__                           \n");
   printf( "                                   \n");
   printf( ";;------------- Main ------------  \n");
   printf( "_main:                             \n");
   printf( "                                   \n");

   Block();
}


//---------------------------------
void Block( void )
//----- <block>       ::= '{' <statement> [ ';' <statement> ]* '}'
{
   Match('{');

   while(1) {

      Statement();

      if ( ';' == Look ) {
         Match(';');
      } else {
         break;
      }
   }

   Match('}');
}


//---------------------------------
void Statement( void )
//----- <statement>   ::= <asm> | <if> | <while> | <break> | <continue> | <assign>
{
   char name[80];

   strcpy(name,  GetName());

   if ( 0 == strcmp(name, "return" ) ) {
       Return();
   } else
   if ( 0 == strcmp(name, "asm" ) ) {
       Asm();
   } else
   if ( 0 == strcmp(name, "if" ) ) {
       If();
   } else
   if ( 0 == strcmp(name, "while" ) ) {
       While();
   } else
   if ( 0 == strcmp(name, "break" ) ) {
       Break();
   } else
   if ( 0 == strcmp(name, "continue" ) ) {
       Continue();
   } else
   if (0 == strcmp(name, "do" ) ){
     Do();
   }else {
      Assign( name );
    }

}


//---------------------------------
void Return( void )
//----  <return>        ::= "return" '(' <expression> ')'
{
   //-- we already have name "return".

   Match( '(' );

   Expression();

   Match( ')' );

   //----------------------------
   //-- Emit code to leave function:
   //--
   //-- stack frame:
   //--        R1    <=== BP
   //--        R2
   //--        R3
   //--        R5, old BP
   //--        R7, ret addr
   //--
   //--   ret val is in R0
   //--   SP = BP
   //--   pop regs
   //--   return to caller

   printf("__TEXT__                                 \n" );
   printf("                   ;;-- LEAVE:           \n" );
   printf("  ADD R6, R5, #0      ;;-- SP = BP       \n" );
   printf("  LDR R1, R6, #0      ;;-- pop           \n" );
   printf("  ADD R6, R6, #1      ;;--   R1          \n" );
   printf("  LDR R2, R6, #0      ;;-- pop           \n" );
   printf("  ADD R6, R6, #1      ;;--   R2          \n" );
   printf("  LDR R3, R6, #0      ;;-- pop           \n" );
   printf("  ADD R6, R6, #1      ;;--   R3          \n" );
   printf("  LDR R5, R6, #0      ;;-- pop old BP    \n" );
   printf("  ADD R6, R6, #1      ;;--   R5          \n" );
   printf("  LDR R7, R6, #0      ;;-- pop ret addr  \n" );
   printf("  ADD R6, R6, #1      ;;--   R7          \n" );
   printf("  JMP R7              ;;-- return        \n" );
}


//---------------------------------
void Asm( void )
//----- <asm>       ::= "asm" '{' <verbatim> '}'
{
   size_t  n;
   char   *line;

   //-- we already have name "asm".

   Match( '{' );

   //------------- <verbatim>, copy line-by-line, verbatim:
   while(1) {

      if( '}' == Look ) { Match( '}' ); break; }

      inLine++; inCol = 0;

      n = 0; line = NULL;
      if (-1 == getline(&line, &n, stdin) ) Abort( "reading asm line failed" );

      printf("%c%s" , Look, line );
      free( line );

      SkipToNextChar();
   }

}


//---------------------------------
void If( void )
//----- <if>          ::= "if" '(' <expression> ')' <block> "else" <block>
//-----         semantics: if ( <expr> < 0 ) then ... else ...
{

   char L1[80];   //-- label for "else" part.
   char L2[80];   //-- label for "endif" part.
   char *s;

   //-- we already have name "if".
   strcpy(L1, nextLabel());
   strcpy(L2, nextLabel());

   printf( "__TEXT__                          \n");
   printf( "                  ;;-- IF         \n");           //-- "if"

   Match('(');                                                //-- "("
   Expression();                                              //-- <expr> value is on stack.
   Match(')');                                                //-- ")"

   printf( "__TEXT__                          \n");
   printf( "  BRzp %s         ;;-- >= 0 ?  go ELSE \n", L1);

   Block();                                                   //--     <block>

   printf( "__TEXT__                          \n");
   printf( "  BRnzp  %s      ;;-- goto ENDIF  \n" , L2);      //--     goto endif

   s = GetName();                                             //-- "else"
   if ( 0 != strcmp(s, "else") ) Abort("no 'else' clause");
   printf( "__TEXT__                          \n");
   printf( "%s:              ;;-- ELSE        \n" , L1);

   Block();                                                   //--     <block>

   printf( "__TEXT__                          \n");
   printf( "%s:              ;;-- ENDIF       \n" , L2);      //--  endif

}


//---------------------------------
void While( void )
//----- <while>       ::= "while" '(' <expression> ')' <block>
//-----                    semantics: (<expr> < 0) == TRUE
{

   char start_while[80];
   char cease_while[80];

   //-- we already have name "while".
   sprintf( start_while, "_start_while_%s" , nextLabel() );
   sprintf( cease_while, "_cease_while_%s" , nextLabel() );

   pushLoopContext( "while", start_while, cease_while );

   printf( "__TEXT__                          \n");
   printf( "                  ;;-- WHILE      \n");           //-- "while"

   Match( '(' );

   printf("__TEXT__            \n"  );
   printf("%s:                 \n" , start_while );

   Expression();

   printf("__TEXT__            \n"  );
   printf("BRzp %s              \n" , cease_while );

   Match( ')' );

   Block();

   printf("__TEXT__            \n"  );
   printf("BRnzp %s            \n" , start_while );
   printf("%s:                 \n" , cease_while );

   popLoopContext();

}


//---------------------------------
void Break( void )
//----- <break>       ::= "break"
{
   //-- we already have name "break".
   if( loopContext == NULL ) Abort( "break statement out of context" );

   printf( "__TEXT__                          \n");
   printf( "                  ;;-- BREAK      \n");
   printf("BRnzp %s   \n" , loopContext->ceaseLabel );
}


//---------------------------------
void Continue( void )
//----- <continue>       ::= "continue"
{
   //-- we already have name "continue".
   if( loopContext == NULL ) Abort( "break statement out of context" );

   printf( "__TEXT__                          \n");
   printf( "                  ;;-- CONTINUE   \n");
   printf("BRnzp %s   \n" , loopContext->startLabel );
}


//---------------------------------
void Assign( char * name )
//----- <assign>      ::=  <name>  '='   <expression>
{
   char offset[80];

   //-- we already have name.

   strcpy( offset, Find_ST_Offset( name ) );

   Match('=');
   Expression();

   printf( "__TEXT__                        \n");
   printf(" STR R0, R4, #%s  ;;-- store %s  \n"
                      ,  offset, name);
}


//---------------------------------
void Expression( void )
//----------------- <expression> ::= <term> [ <op> <term>]*
//-----------------         <op> ::= '+' | '-' | '&' | '|' | '=' | '>' | '<'
//---
//--- Coge generated leaves value of expression in R0.
//---
//-----------------                      | '~'
{
   char L[80];   //-- label

   Term();

   while ( (Look == '+') | (Look == '-') | (Look == '&') | (Look == '|')
                         | (Look == '=') | (Look == '>') | (Look == '<')
                         | (Look == '~')
         )
   {

      switch( Look ) {

       case '+': Match('+');
                 printf( "__TEXT__                       \n");
                 push("R0");
                 Term();
                 printf( "__TEXT__                       \n");
                 pop("R1");
                 printf(" ADD R0, R1, R0   ;;-- ADD      \n");
                 break;

       case '-': Match('-');
                 printf( "__TEXT__                       \n");
                 push("R0");
                 Term();
                 printf( "__TEXT__                       \n");
                 pop("R1");
                 negate("R0");
                 printf(" ADD R0, R1, R0   ;;-- SUB      \n");
                 break;

       case '&': Match('&');
                 printf( "__TEXT__                       \n");
                 push("R0");
                 Term();
                 printf( "__TEXT__                       \n");
                 pop("R1");
                 printf(" AND R0, R1, R0    ;;-- AND     \n");
                 break;

       case '|': Match('|');
                 printf( "__TEXT__                       \n");
                 push("R0");
                 Term();
                 printf( "__TEXT__                       \n");
                 pop("R1");
                 printf(" NOT R0, R0       ;;-- OR       \n");
                 printf(" NOT R1, R1       ;;-- OR       \n");
                 printf(" AND R0, R1, R0   ;;-- OR       \n");
                 printf(" NOT R0, R0       ;;-- OR       \n");
                 break;

       case '=': Match('=');
                 strcpy(L, nextLabel());
                 printf( "__TEXT__                       \n");
                 push("R0");
                 Term();
                 printf( "__TEXT__                       \n");
                 printf(" ADD R2, R0, #0   ;;-- = R2 <== R0\n");
                 pop("R1");
                 printf(" AND R0, R0, #0   ;;-- = set     \n");
                 printf(" NOT R0, R0       ;;-- =   R0 = T\n");
                 negate("R2");
                 printf(" ADD R1, R1, R2   ;;-- =         \n");
                 printf(" BRz %s           ;;-- =         \n", L);
                 printf(" NOT R0, R0       ;;-- =   R0 = F\n");
                 printf("%s:               ;;-- =         \n", L);
                 printf(" AND R0, R0, R0   ;;-- = set CC  \n");
                 break;

       case '>': Match('>');
                 strcpy(L, nextLabel());
                 printf( "__TEXT__                       \n");
                 push("R0");
                 Term();
                 printf( "__TEXT__                       \n");
                 printf(" ADD R2, R0, #0   ;;-- > R2 <== R0\n");
                 negate("R2");
                 pop("R1");
                 printf(" AND R0, R0, #0   ;;-- > set     \n");
                 printf(" NOT R0, R0       ;;-- >   R0 = T\n");
                 printf(" ADD R1, R1, R2   ;;-- >         \n");
                 printf(" BRp %s           ;;-- >         \n", L);
                 printf(" NOT R0, R0       ;;-- >   R0 = F\n");
                 printf("%s:               ;;-- >         \n", L);
                 printf(" AND R0, R0, R0   ;;-- > set CC  \n");
                 break;

       case '<': Match('<');
                 strcpy(L, nextLabel());
                 printf( "__TEXT__                          \n");
                 push("R0");
                 Term();
                 printf( "__TEXT__                          \n");
                 printf(" ADD R2, R0, #0   ;;-- < R2 <== R0 \n");
                 negate("R2");
                 pop("R1");
                 printf(" AND R0, R0, #0   ;;-- < set       \n");
                 printf(" NOT R0, R0       ;;-- <  R0 = T   \n");
                 printf(" ADD R1, R1, R2   ;;-- <           \n");
                 printf(" BRn %s           ;;-- <           \n", L);
                 printf(" NOT R0, R0       ;;-- <           \n");
                 printf("%s:               ;;-- <           \n", L);
                 printf(" AND R0, R0, R0   ;;-- <  set CC   \n");
                 break;

       case '~': Match('~');
                 printf( "__TEXT__              ~           \n");
                 push("R0");
                 Term();
                 printf( "__TEXT__                          \n");
                 printf(" ADD R2, R0, #0   ;;-- ~ R2 <== R0 \n");
                 pop("R1");
                 printf(" AND R0, R1, R2   ;;-- ~           \n");
                 printf(" NOT R0, R0       ;;-- ~           \n");
                 printf(" NOT R1, R1       ;;-- ~           \n");
                 printf(" NOT R2, R2       ;;-- ~           \n");
                 printf(" AND R3, R1, R2   ;;-- ~           \n");
                 printf(" NOT R3, R3       ;;-- ~           \n");
                 printf(" AND R0, R0, R3   ;;-- ~           \n");
                 break;

       default: Abort("expression missing operator");
      }

   }
}


//---------------------------------
void Term( void )
//----- <term>          ::= <numconst> | <strconst> | '(' <expression> ')'
//-----                                | '*' <var>  | '&' <var> | <ident>
{
   char name[80];
   char *offset;

   if ( isdigit( Look ) ) {
      NumConst();
   }
   else
   if ( '"' == Look ) {
      StrConst();
   }
   else {
   if ( '(' == Look ) {
      Match('(');
      Expression();
      Match(')');
   }
   else
   if ( '*' == Look ) {
      Match('*');
      strcpy( name, GetName() );
      offset = Find_ST_Offset( name );
      printf( "__TEXT__                          \n");
      printf( "         ;;-- deref. var          \n");
      printf( " LDR R0, R4, #%s   ;;-- get %s    \n", offset, name );
      printf( " LDR R0, R0, #0    ;;-- deref.    \n");
   }
   else
   if ( '&' == Look ) {
      Match('&');
      strcpy( name, GetName() );
      offset = Find_ST_Offset( name );
      printf( "__TEXT__                          \n");
      printf( "         ;;-- ref. var            \n");
      printf( " ADD R0, R4, #%s   ;;-- get & %s  \n", offset, name );
   }
   else
   if ( isalpha( Look ) ) {
      Ident();
   }
   else
      Abort("unknown term syntax");
   }

}


//---------------------------------
void NumConst( void )
//-------------       <numconst>  ( data def and push )
{

   char *num;
   num = GetNum();

   printf( "__TEXT__                           \n");
   printf( " LDR R0, R4, #%d   ;;-- get const  \n", GDT_offset );

   printf( "__DATA__                                    \n");
   printf( "_GDT_%d:    .FILL #%s          ;;--- const  \n", GDT_offset,  num);

   GDT_offset++;
}


//---------------------------------
void StrConst( void )
//-------------       <strconst>  ::= '"' [ <char> ]* '"'
{

   char s[80];
   char s1[80] = "";
   char label[80];
   char ch;

   //---- we already have '"'.

   //--- not doing Match( '"' ) because we do our own getc().

   while( '"' != (ch = getc(stdin)) ) {

       if ( EOF == ch ) Abort( "No closing quote" );

       sprintf( s , "%c", ch);   //--- Make ch into a string s.
       strcat(  s1, s);          //--- Add's ch to end of s1.
   }


   sprintf( label, "_RD_%d", RD_index++);

   printf( "__RODATA__                             \n");
   printf( "%s:    .STRINGZ \"%s\"    ;;--- const  \n", label,  s1);

   printf( "__TEXT__                           \n");
   printf( " LD  R0, %s_addr   ;;-- get addr   \n", label );
   printf( " BRnzp %s_done                     \n", label );
   printf( "%s_addr:   .FILL %s                \n", label, label );
   printf( "%s_done:                           \n", label );

   //--- get Look = '"' so we can do Match( '"' );
   Look = '"';

   Match( '"' );

}


//---------------------------------
void Ident( void )
//----- <ident>       ::= <funCall> | <varRef> | <asm>
{
   char name[80];
   strcpy( name, GetName() );

   if( Look == '(' ) {
      FunCall(name);
   } else
   if( 0 == strcmp( name, "asm" ) ) {
      Asm();
   } else {
      VarRef(name);
   }
}


//---------------------------------
void FunCall( char * name )
//---------  <funcall> ::= <name> '(' <expression> ')'
{
   //-- we already have the name, check syntax:

   Match( '(' );
   Expression();
   Match( ')' );

   printf( "__TEXT__                          \n");
   printf("  LDR R7, R4, #%s  ;;-- R7 <== &%s \n", Find_ST_Offset( name ), name );
   printf("  JSRR R7          ;;-- call %s    \n", name );

}


//---------------------------------
void VarRef( char * name )
//----  <varRef>        ::= <name> | '*' <name> | '&' <name>
//---- we already have the name or '*' or '&'.
{
   char *offset;

   offset = Find_ST_Offset( name );

   //-- get var's value.
   printf( "__TEXT__                        \n");
   printf( " LDR R0, R4, #%s   ;;-- get %s  \n", offset, name );
}


//---------------------------------
void GlobalDecls( void )
//----  <globalDecls> ::= ( <globalVarDecl> | <functionDecl>  |  <asm> )
//----                    [ ';' ( <globalVarDecl> | <functionDecl>  |  <asm> ) ]*
{
   char type[80];
   char name[80];

   while(1) {

      strcpy( type, GetName() );

      if ( 0 == strcmp(type, "asm" ) ) {

         Asm();

      } else {

         strcpy( name, GetName() );

         if( Look == '(' ) {
            FuncDecl( type, name );
         } else {
            GlobalVarDecl( type, name );
         }
      }

      if ( ';' == Look ) {
         Match(';');
      } else {
         break;
      }
   }
}


//---------------------------------
void GlobalVarDecl( char * type , char * name )
//----  <globalVarDecl> ::=  <type> <name>
{

   //-- we already have the type and name.

   if ( 0 == strcmp(type, "int" ) ) {

     //-- put var in to ST:

     AbortIfDuplicateSymbol( name );
     putVarIn_ST( name, type );

     //-- Make new entry in Global Data Table:

     printf( "__DATA__                          \n");
     printf( "_%s:    .BLKW 1         ;;-- var  \n", name );

   } else {
       Abort("unknown type in variable declaration");
   }
}

//---------------------------------
void FuncDecl( char * type, char * name )
//----  <funcDecl>      ::= <type> <name> '(' <type> <name> ')' <block>
{

   char arg_name[80];
   char arg_type[80];
   char label[80];
   char *offset;

   if ( 0 != strcmp(type, "int" ) )  Abort("unknown type in function declaration");

   Match( '(' );

   strcpy( arg_type, GetName() );
   strcpy( arg_name, GetName() );
   GlobalVarDecl( arg_type, arg_name );

   Match( ')' );

   //-- We already have the type and func name.
   //-- Enter function in ST, make function pointer
   //-- variable, and emit entry label.

   AbortIfDuplicateSymbol( name );
   putVarIn_ST( name, type );


   sprintf( label, "_%s_entry", name );

   printf(      "__DATA__                              \n");
   printf(      "_%s:    .FILL %s          ;;-- f_ptr  \n", name, label );

   printf(      "__TEXT__                        \n");
   printf(      "%s:             ;;--   BEGIN-%s \n", label, name );

   //-----------------------------------
   //-- Emit code for function preamble:
   //--   move arg val. to arg var.
   //--   push return address, R7;
   //--   push regs R5, R3, R2, R1;
   //--   set BP = SP;
   //--
   //-- new stack frame:
   //--        R1    <=== BP, SP
   //--        R2
   //--        R3
   //--        R5, old BP
   //--        R7, ret addr
   //--
   //-- Get arg var's offset.
   //-- Copy arg val to arg var.
   //-- Save regs; set new BP

   offset = Find_ST_Offset( arg_name );

   printf( "__TEXT__                           \n");
   printf( "                ;;-- ENTER:        \n");
   printf("  STR R0, R4, #%s    ;;-- R0  ===> arg var. \n" , offset);
   printf("  ADD R6, R6, #-1    ;;-- push ret addr \n" );
   printf("  STR R7, R6, #0     ;;--    R7         \n" );
   printf("  ADD R6, R6, #-1    ;;-- push old BP   \n" );
   printf("  STR R5, R6, #0     ;;--    R5         \n" );
   printf("  ADD R6, R6, #-1    ;;-- push          \n" );
   printf("  STR R3, R6, #0     ;;--    R3         \n" );
   printf("  ADD R6, R6, #-1    ;;-- push          \n" );
   printf("  STR R2, R6, #0     ;;--    R2         \n" );
   printf("  ADD R6, R6, #-1    ;;-- push          \n" );
   printf("  STR R1, R6, #0     ;;--    R1         \n" );
   printf("  ADD R5, R6, #0     ;;-- new BP = SP   \n" );

   //-- Emit code for function body:

   Block();

   //-- mark end of func body:

   printf(         "__TEXT__   \n");
   printf( "                   ;;--   END-%s\n", name );

}


//---------------------------------
void Header( void )
//----  <header>        ::= 'HEADER' <category>
{
   char name[80];
   char category[80];
   char load_addr[80];
   char stack_bot[80];

   strcpy( name, GetName() );
   if ( 0 != strcmp( name, "HEADER") ) Abort( "Header missing" );

   strcpy( category, GetName() );

   if ( 0 == strcmp( category, "USER") ) {
       sprintf( load_addr, "x1000" );
       sprintf( stack_bot, "x8000" );
   } else
   if ( 0 == strcmp( category, "OS"  ) ) {
       strcpy( load_addr, "x8000" );
       strcpy( stack_bot, "xC000" );
   } else
   if ( 0 == strcmp( category, "BOOT") ) {
       strcpy( load_addr, "x0200" );
       strcpy( stack_bot, "xC000" );
   } else {
      Abort( "Header category missing or unknown" );
   }

   printf("__TEXT__                            \n");
   printf( ";;======= Header ================= \n");
   printf(".ORIG %s                            \n", load_addr);
   printf( "                                   \n");
   printf( ";;======= Begin TEXT Segment ===== \n");
   printf( ";;-------- Preamble -------------  \n");
   printf( "  LD R4, _GDT_address              \n");
   printf( "  LD R6, _stack_bottom             \n");
   printf( "  LD R5, _stack_bottom             \n");
   printf( "  LD R7, _main_address             \n");
   printf( "  JMP R7                           \n");
   printf( "_GDT_address:   .FILL _GDT         \n");
   printf( "_stack_bottom:  .FILL %s           \n", stack_bot);
   printf( "_main_address:  .FILL _main        \n");
   printf( "                                   \n");

   printf( "__DATA__                           \n");
   printf( "                                   \n");
   printf( ";;==== Begin DATA Segment ======== \n");
   printf( "                                   \n");
   printf( "_GDT:                              \n");
}


//------------------------------
void Postamble( void )
//---------  Emit standard program postamble.
{
   printf( "__RODATA__                         \n");
   printf( "                                   \n");
   printf( ".END\n");
}


//=================================
//== Main
//=================================
int main(void)
{
    Init();

    Header();
    Program();
    Postamble();

    return(0);
}
