{
module Parser where

import qualified Formula as THF
import qualified Statement as Statement
import Statement (Statement)
import Data.Char
import Data.Data
import Data.Ratio
import Control.Monad
import Data.List as L
import Lexer
import Data.Set as S
import System.IO
import System.IO.Unsafe
import Control.Monad.Identity
}

%name parseTHF
%tokentype { Token }
%error {

          ((\xs -> case xs of
                    xs -> error ("Parse error, pos: "++show (take 25 xs))))
       }


%token
 tok_lp                 { LP }
 tok_rp                 { RP }
 tok_lbra               { Lbrack }
 tok_rbra               { Rbrack }
 tok_comma              { Comma }
 tok_dot                { Dot }
 tok_star               { Star }
 tok_plus               { Plus }
 tok_rangle             { Rangle }
 tok_colon              { Oper ":" }

 tok_iff                { Oper "<=>"}
 tok_implies            { Oper "=>"}
 tok_xor                { Oper "<~>"}
 tok_nor                { Oper "~|"}
 tok_nand               { Oper "~&"}
 tok_impliedby          { Oper "<=" }
 tok_equals             { Oper "=" }
 tok_nequals            { Oper "!=" }

 tok_dbl_exclam             { Oper "!!" }
 tok_dbl_question           { Oper "??" }

 tok_subtype           { Oper "<<" }

tok_exclam             { Oper "!" }
 tok_question           { Oper "?" }
 tok_ampersand          { Oper "&" }
 tok_vline              { Oper "|" }
 tok_tilde              { Oper "~" }
 tok_carrot              { Oper "^" }
 tok_at             { Oper "@" }

tok_mystery1 { Oper "!>" }
tok_mystery2 { Oper "?*" }
tok_mystery3 { Oper "@+" }
tok_mystery4 { Oper "@-" }


 tok_thf                { LowerWord "thf" }
 tok_include_           { LowerWord "include" }

 tok_single_quoted      { SingleQuoted $$ }
 tok_distinct_object    { DoubleQuoted $$ }
 tok_dollar_word        { DollarWord $$ }
 tok_dollar_dollar_word { DollarDollarWord $$ }
 tok_upper_word         { UpperWord $$ }
 tok_lower_word         { LowerWord $$ }
 tok_signed_integer     { SignedInt $$ }
 tok_unsigned_integer   { UnsignedInt $$ }
 tok_real               { Real $$ }
 tok_slash              { Slash }

 comment            { CommentToken $$ }

%%

TPTP_file  :: {[Statement]}
TPTP_file  : {[]} | TPTP_input TPTP_file  {$1 : $2}

TPTP_input  :: {Statement}
TPTP_input  : annotated_formula  {$1}
             | include  { $1 }
             | comment { Statement.Comment }

annotated_formula  :: {Statement}
annotated_formula  :  thf_annotated {$1}

thf_annotated  :: {Statement}
thf_annotated  : thf lp name comma formula_role  comma thf_formula  annotations rp dot
       { Statement.AnnotatedFormula $3 $5 $7 }

annotations  :  comma source optional_info  { }
| {- empty -} {  }

formula_role  :: {Statement.Role}
formula_role  : lower_word_ { Statement.Role $1 }

-- THF Formula

thf_formula :: { THF.Formula }
thf_formula : thf_logic_formula { $1 }

thf_logic_formula :: { THF.Formula }
thf_logic_formula : thf_binary_formula { $1 }
| thf_unitary_formula { $1 }
| thf_type_formula { $1 }
| thf_subtype { $1 }

thf_binary_formula :: { THF.Formula }
thf_binary_formula : thf_binary_pair { $1 }
| thf_binary_tuple { $1 }
| thf_binary_type { $1 }

thf_binary_pair :: { THF.Formula }
thf_binary_pair : thf_unitary_formula thf_pair_connective thf_unitary_formula { THF.BinaryFormula $1 $2 $3 }

thf_binary_tuple :: { THF.Formula }
thf_binary_tuple : thf_or_formula { $1 }
| thf_and_formula { $1 }
| thf_apply_formula { $1 }

thf_or_formula :: { THF.Formula }
thf_or_formula : thf_unitary_formula vline thf_unitary_formula { THF.OrFormula $1 $3 }
| thf_or_formula vline thf_unitary_formula { THF.OrFormula $1 $3 }

thf_and_formula :: { THF.Formula }
thf_and_formula : thf_unitary_formula ampersand thf_unitary_formula { THF.AndFormula $1 $3 }
| thf_and_formula ampersand thf_unitary_formula { THF.AndFormula $1 $3 }

thf_apply_formula :: { THF.Formula }
thf_apply_formula : thf_unitary_formula at thf_unitary_formula { THF.ApplyFormula $1 $3}
| thf_apply_formula at thf_unitary_formula { THF.ApplyFormula $1 $3 }

thf_unitary_formula :: { THF.Formula }
thf_unitary_formula : thf_quantified_formula { $1 }
| thf_unary_formula { $1 }
| thf_atom { $1 }
| thf_conditional { $1 }
| lp thf_logic_formula rp { $2 }

thf_quantified_formula :: { THF.Formula }
thf_quantified_formula : thf_quantifier lbra thf_variable_list rbra colon thf_unitary_formula { THF.QuantifiedFormula $1 $3 $6 }

thf_variable_list :: { [THF.Var] }
thf_variable_list : thf_variable { [$1] }
| thf_variable comma thf_variable_list { $1 : $3 }

thf_variable :: { THF.Var }
thf_variable : thf_typed_variable { $1 }
| variable { THF.UntypedVar $1 }

thf_typed_variable :: { THF.Var }
thf_typed_variable : variable colon thf_top_level_type { THF.TypedVar $1 $3 }

thf_unary_formula :: { THF.Formula }
thf_unary_formula : thf_unary_connective lp thf_logic_formula rp { THF.UnaryFormula $1 $3 }

thf_atom :: { THF.Formula }
thf_atom : thf_term { $1 }
| thf_conn_term { $1 }

thf_conditional :: { THF.Formula }
thf_conditional : dollar_word lp thf_logic_formula comma thf_logic_formula comma thf_logic_formula rp
{ THF.Conditional $3 $5 $7 }

thf_type_formula :: { THF.Formula }
thf_type_formula : thf_typeable_formula colon thf_top_level_type { THF.TypeFormula $1 $3 }

thf_typeable_formula :: { THF.Formula }
thf_typeable_formula : thf_atom { $1 }
| lp thf_logic_formula rp { $2 }

thf_subtype :: { THF.Formula }
thf_subtype : constant subtype_sign constant { THF.Subtype $1 $3 }

thf_top_level_type :: { THF.Formula }
thf_top_level_type : thf_logic_formula { $1 }

thf_unitary_type :: { THF.Formula }
thf_unitary_type : thf_unitary_formula { $1 }

thf_binary_type :: { THF.Formula }
thf_binary_type : thf_mapping_type { $1 }
| thf_xprod_type { $1 }
| thf_union_type { $1 }

thf_mapping_type :: { THF.Formula }
thf_mapping_type : thf_unitary_type rangle thf_unitary_type { THF.Mapping $1 $3 }
| thf_unitary_type rangle thf_mapping_type { THF.Mapping $1 $3 }

thf_xprod_type :: { THF.Formula }
thf_xprod_type : thf_unitary_type star thf_unitary_type { THF.XProp $1 $3 }
| thf_xprod_type star thf_unitary_type { THF.XProp $1 $3 }

thf_union_type :: { THF.Formula }
thf_union_type : thf_unitary_type plus thf_unitary_type { THF.Union $1 $3 }
| thf_union_type plus thf_unitary_type { THF.Union $1 $3 }

thf_conn_term :: { THF.Formula }
thf_conn_term : thf_pair_connective { $1 }
| assoc_connective { $1 }
| thf_unary_connective { $1 }

thf_quantifier :: { THF.Quantifier }
thf_quantifier : exclam { THF.Q_Exclam }
| question { THF.Q_Question }
| carrot { THF.Q_Carrot }
| mystery1 { THF.Q_Mystery1 }
| mystery2 { THF.Q_Mystery2 }
| mystery3 { THF.Q_Mystery3 }
| mystery4 { THF.Q_Mystery4 }

thf_pair_connective :: { THF.Formula }
thf_pair_connective : infix_equality { $1 }
| infix_inequality { $1 }
| binary_connective { $1 }

thf_unary_connective :: { THF.Formula }
thf_unary_connective : unary_connective { $1 }
| dbl_exclam { THF.Operator THF.PI }
| dbl_question { THF.Operator THF.EX }

subtype_sign : subtype {}

-- END THF

binary_connective  :: {THF.Formula}
binary_connective  : iff { THF.Operator THF.IFF }
                    | implies { THF.Operator THF.Implies }
                    | impliedby { THF.Operator THF.ImpliedBy }
                    | xor { THF.Operator THF.XOR }
                    | nor  { THF.Operator THF.NOR }
                    | nand { THF.Operator THF.NAND }

assoc_connective  : vline { THF.Operator THF.OR }
                  | ampersand { THF.Operator THF.AND }

unary_connective  :: { THF.Formula }
unary_connective  : tilde { THF.Operator THF.NOT }

-- defined_type  :== atomic_defined_word

-- defined_type  :== $oType | $o | $iType | $i | $tType | $real | $int
-- system_type  :== atomic_system_word


infix_equality  :: { THF.Formula }
infix_equality  : equals { THF.Operator THF.EQ }

infix_inequality  :: { THF.Formula }
infix_inequality  : nequals { THF.Operator THF.NEQ }

thf_term  :: {THF.Formula}
thf_term  : function_term  {$1}
| variable        {THF.Variable (THF.UntypedVar $1)}

function_term  :: {THF.Formula}
function_term  : plain_term {$1}
               | defined_term  {$1}
               | system_term {$1}

plain_term  :: {THF.Formula}
plain_term  :  constant                  {THF.Constant $1 }
| functor  lp arguments  rp {THF.Functor $1 $3}

constant  :: {String}
constant  : functor {$1}

functor  :: {String}
functor  : atomic_word {$1}

defined_term  :: {THF.Formula}
defined_term  : defined_atom { $1 }
              | defined_atomic_term { $1 }

defined_atom  :: {THF.Formula}
defined_atom  : number { THF.Number $1 }
| distinct_object { THF.Object $1 }

defined_atomic_term :: {THF.Formula}
defined_atomic_term  : defined_plain_term { $1 }

defined_plain_term  :: {THF.Formula}
defined_plain_term  : defined_constant { THF.Constant $1 }
| defined_functor  lp arguments  rp { THF.Functor $1 $3  }

defined_constant  :: {String}
defined_constant  : defined_functor {$1}
-- defined_constant  :==

defined_functor  :: {String}
defined_functor  : atomic_defined_word {$1}
-- defined_functor  :==

system_term  :: {THF.Formula}
system_term  :  system_constant  {THF.Constant $1}
| system_functor  lp arguments  rp {THF.Functor $1 $3}

system_constant  :: {String}
system_constant  : system_functor  {$1}

system_functor  :: {String}
system_functor  : atomic_system_word {$1}

variable  :: {String}
variable  : upper_word {$1}

arguments  :: {[THF.Formula]}
arguments  : thf_term  {[$1]}
           | thf_term  comma arguments  { $1 : $3 }

source  :: {}
source  : general_term  {}

optional_info  :: {}
optional_info  :  comma useful_info  {$2} |  {}

useful_info  :: { }
useful_info  : general_list  {}

include :: {Statement}
include  : include_ lp file_name formula_selection  rp dot { Statement.Include $3 $4 }

formula_selection  :: {[String]}
formula_selection  :  comma lbra name_list  rbra { $3 }
                    |   { [] }

name_list  :: {[String]}
name_list  : name  {[$1]}
            | name  comma name_list  { $1 : $3 }

general_term  :: {}
general_term  :  general_data  {}
               | general_data colon general_term  {}
               | general_list {}

general_data  :: {}
general_data  :  atomic_word  { }
               | atomic_word  lp general_terms  rp {}
               | variable  { }
               | number  { }
               | distinct_object {  }
               | formula_data  {  }

formula_data :: {}
formula_data  : dollar_word lp thf_formula  rp { }

general_list  :: {}
general_list  : lbra rbra {}
               | lbra general_terms  rbra {}

general_terms  :: {}
general_terms  :  general_term  {}
                | general_term  comma general_terms  {}

name  :: {String}
name  : atomic_word  {$1}
      | unsigned_integer {show $1}

atomic_word  :: {String}
atomic_word  : lower_word_ { $1}
              | single_quoted{stripQuotes '\'' $1}

atomic_defined_word  :: {String}
atomic_defined_word  : dollar_word{$1}

atomic_system_word  :: {String}
atomic_system_word  : dollar_dollar_word{$1}

number  :: {Rational} -- maybe keep track of the number type that was actually parsed
number  : integer {fromIntegral $1} | rational {$1} | real {$1}

integer :: {Integer}
integer : signed_integer {$1} | unsigned_integer {$1}

rational :: {Rational}
rational : integer tok_slash unsigned_integer {$1 % $3}

file_name  :: {String}
file_name  : single_quoted {stripQuotes '\'' $1}

lower_word_ :: {String}
lower_word_ : lower_word {$1} | thf {"thf"} | include_ {"include"} -- "thf" is a perfectly cromulent lower_word, but it is interpreted as a "thf" token

lp                 :: {Token}
lp                 : tok_lp                  comment_list { $1 }
rp                 :: {Token}
rp                 : tok_rp                  comment_list { $1 }
lbra               :: {Token}
lbra               : tok_lbra                comment_list { $1 }
rbra               :: {Token}
rbra               : tok_rbra                comment_list { $1 }
comma              :: {Token}
comma              : tok_comma               comment_list { $1 }
dot                :: {Token}
dot                : tok_dot                 comment_list { $1 }
star               :: {Token}
star               : tok_star                comment_list { $1 }
plus               :: {Token}
plus               : tok_plus                comment_list { $1 }
rangle             :: {Token}
rangle             : tok_rangle              comment_list { $1 }
colon              :: {Token}
colon              : tok_colon               comment_list { $1 }

iff                :: {Token}
iff                : tok_iff                 comment_list { $1 }
implies            :: {Token}
implies            : tok_implies             comment_list { $1 }
xor                :: {Token}
xor                : tok_xor                 comment_list { $1 }
nor                :: {Token}
nor                : tok_nor                 comment_list { $1 }
nand               :: {Token}
nand               : tok_nand                comment_list { $1 }
impliedby          :: {Token}
impliedby          : tok_impliedby           comment_list { $1 }
equals             :: {Token}
equals             : tok_equals              comment_list { $1 }
nequals            :: {Token}
nequals            : tok_nequals             comment_list { $1 }

dbl_exclam         :: {Token}
dbl_exclam         : tok_dbl_exclam              comment_list { $1 }
dbl_question       :: {Token}
dbl_question       : tok_dbl_question            comment_list { $1 }
subtype            :: {Token}
subtype            : tok_subtype                 comment_list { $1 }

exclam             :: {Token}
exclam             : tok_exclam              comment_list { $1 }
question           :: {Token}
question           : tok_question            comment_list { $1 }
ampersand          :: {Token}
ampersand          : tok_ampersand           comment_list { $1 }
vline              :: {Token}
vline              : tok_vline               comment_list { $1 }
tilde              :: {Token}
tilde              : tok_tilde               comment_list { $1 }
carrot             :: {Token}
carrot             : tok_carrot               comment_list { $1 }
at                 :: {Token}
at                 : tok_at               comment_list { $1 }

mystery1           :: {Token}
mystery1           : tok_mystery1          comment_list { $1 }
mystery2           :: {Token}
mystery2           : tok_mystery2          comment_list { $1 }
mystery3           :: {Token}
mystery3           : tok_mystery3          comment_list { $1 }
mystery4           :: {Token}
mystery4           : tok_mystery4          comment_list { $1 }

thf                :: {Token}
thf                : tok_thf                 comment_list { $1 }

include_           :: {Token}
include_           : tok_include_            comment_list { $1 }

single_quoted      :: {String}
single_quoted      : tok_single_quoted       comment_list { $1 }
distinct_object    :: {String}
distinct_object    : tok_distinct_object     comment_list { $1 }
dollar_word        :: {String}
dollar_word        : tok_dollar_word         comment_list { $1 }
dollar_dollar_word :: {String}
dollar_dollar_word : tok_dollar_dollar_word  comment_list { $1 }
upper_word         :: {String}
upper_word         : tok_upper_word          comment_list { $1 }
lower_word         :: {String}
lower_word         : tok_lower_word          comment_list { $1 }
signed_integer     :: {Integer}
signed_integer     : tok_signed_integer      comment_list { $1 }
unsigned_integer   :: {Integer}
unsigned_integer   : tok_unsigned_integer    comment_list { $1 }
real               :: {Rational}
real               : tok_real                comment_list { $1 }

comment_list :: {[String]}
comment_list : {[]} | comment comment_list { $1 : $2 }

{

stripQuotes which (x:xs) = go xs
                      where
                        go [x] = []
                        go ('\\':'\\':xs) = '\\':go xs
                        go ('\\':which:xs) = which:go xs
                        go (x:xs) = x:go xs


}
