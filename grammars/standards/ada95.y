/* Ada 95 grammar 
 * Taken from the Ada 95 syntax summary (Annex P) at:
 *   http://www.adahome.com/Resources/refs/grammar.html
 */



/* reserved words */

%token abort abs abstract accept access aliased all and array at 
%token begin body case constant declare delay delta digits do 
%token else elsif end  entry exception exit for function 
%token generic goto if in is limited loop mod 
%token new not null of or others out 
%token package pragma private procedure protected 
%token raise range record rem renames requeue return reverse 
%token select separate subtype tagged task terminate then type 
%token until use when while with xor 


/* Attributes (formally, these are not reserved words) */
%token Access Delta Digits Range 

/* 2.3 */
%token identifier
/* 2.4 */
%token numeric_literal 
/* 2.5 */
%token character_literal
/* 2.6 */
%token string_literal

%%

/* 3.1 */
basic_declaration :
     type_declaration                 | subtype_declaration
   | object_declaration               | number_declaration
   | subprogram_declaration           | abstract_subprogram_declaration
   | package_declaration              | renaming_declaration
   | exception_declaration            | generic_declaration
   | generic_instantiation
  ;
/* 3.1 */
defining_identifier : identifier
  ;
/* 3.2.1 */
type_declaration :  full_type_declaration
   | incomplete_type_declaration
   | private_type_declaration
   | private_extension_declaration
  ;

/* 3.2.1 */
full_type_declaration :
     type defining_identifier [known_discriminant_part] is type_definition ";"
   | task_type_declaration
   | protected_type_declaration
  ;
/* 3.2.1 */
type_definition :
     enumeration_type_definition | integer_type_definition
   | real_type_definition        | array_type_definition
   | record_type_definition      | access_type_definition
   | derived_type_definition
  ;
/* 3.2.2 */
subtype_declaration :
   subtype defining_identifier is subtype_indication ";"
  ;
/* 3.2.2 */
subtype_indication :  subtype_mark [constraint]
  ;
/* 3.2.2 */
subtype_mark : name
  ;
/* 3.2.2 */
constraint : scalar_constraint | composite_constraint
  ;
/* 3.2.2 */
scalar_constraint :
     range_constraint | digits_constraint | delta_constraint
  ;
/* 3.2.2 */
composite_constraint :
     index_constraint | discriminant_constraint
  ;
/* 3.3.1 */
object_declaration :
    defining_identifier_list ":" [aliased] [constant]
    subtype_indication [":=" expression] ";" 
  | defining_identifier_list ":" [aliased] [constant]
    array_type_definition [":=" expression] ";" 
  | single_task_declaration
  | single_protected_declaration
  ;
/* 3.3.1 */
defining_identifier_list :
  defining_identifier {"," defining_identifier}
  ;
/* 3.3.2 */
number_declaration :
     defining_identifier_list ":" constant ":=" expression ";"
  ;
/* 3.4 */
derived_type_definition : [abstract] new subtype_indication
[record_extension_part] 
  ;
/* 3.5 */
range_constraint :  range _range
  ;
/* 3.5 */
_range :  range_attribute_reference
   | simple_expression ".." simple_expression
  ;
/* 3.5.1 */
enumeration_type_definition :
   "(" enumeration_literal_specification {"," enumeration_literal_specification} ")"
  ;
/* 3.5.1 */
enumeration_literal_specification :  defining_identifier |
defining_character_literal 
  ;
/* 3.5.1 */
defining_character_literal : character_literal
  ;
/* 3.5.4 */
integer_type_definition : signed_integer_type_definition | modular_type_definition
  ;
/* 3.5.4 */
signed_integer_type_definition : range static_simple_expression ".." static_simple_expression
  ;
/* 3.5.4 */
modular_type_definition : mod expression
  ;
/* 3.5.6 */
real_type_definition :
   floating_point_definition | fixed_point_definition
  ;
/* 3.5.7 */
floating_point_definition :
  digits expression [real_range_specification]
  ;
/* 3.5.7 */
real_range_specification :
  range static_simple_expression ".." static_simple_expression
  ;
/* 3.5.9 */
fixed_point_definition : ordinary_fixed_point_definition | decimal_fixed_point_definition
  ;
/* 3.5.9 */
ordinary_fixed_point_definition :
   delta expression  real_range_specification
  ;
/* 3.5.9 */
decimal_fixed_point_definition :
   delta expression digits expression [real_range_specification]
  ;
/* 3.5.9 */
digits_constraint :
   digits expression [range_constraint]
  ;
/* 3.6 */
array_type_definition :
   unconstrained_array_definition | constrained_array_definition
  ;
/* 3.6 */
unconstrained_array_definition :
   array "(" index_subtype_definition {"," index_subtype_definition}
   ")" of component_definition 
  ;
/* 3.6 */
index_subtype_definition : subtype_mark _range "<>"
  ;
/* 3.6 */
constrained_array_definition :
   array "(" discrete_subtype_definition {","
   discrete_subtype_definition} ")" of component_definition 
  ;
/* 3.6 */
discrete_subtype_definition : subtype_indication | _range
  ;
/* 3.6 */
component_definition : [aliased] subtype_indication
  ;
/* 3.6.1 */
index_constraint :  "(" discrete_range {"," discrete_range} ")"
  ;
/* 3.6.1 */
discrete_range : subtype_indication | _range
  ;
/* 3.7 */
discriminant_part : unknown_discriminant_part | known_discriminant_part
  ;
/* 3.7 */
unknown_discriminant_part : "(" "<>" ")"
  ;
/* 3.7 */
known_discriminant_part :
   "(" discriminant_specification { ";" discriminant_specification} ")"
  ;
/* 3.7 */
discriminant_specification :
   defining_identifier_list ":" subtype_mark [":=" default_expression]
 | defining_identifier_list ":" access_definition [":=" default_expression]
  ;
/* 3.7 */
default_expression : expression
  ;
/* 3.7.1 */
discriminant_constraint :
   "(" discriminant_association {"," discriminant_association} ")"
  ;
/* 3.7.1 */
discriminant_association :
   [selector_name {"|" selector_name} "=>"]
   expression 
  ;
/* 3.8 */
record_type_definition : [[abstract] tagged] [limited] record_definition
  ;
/* 3.8 */
record_definition :
    record
       component_list
    end record
  | null record
  ;
/* 3.8 */
component_list :
      component_item {component_item}
   | {component_item} variant_part
   |  null ";"
  ;
/* 3.8 */
component_item : component_declaration | representation_clause
  ;
/* 3.8 */
component_declaration :
   defining_identifier_list ":" component_definition [":=" default_expression] ";"
  ;
/* 3.8.1 */
variant_part :
   case direct_name is
       variant
      {variant}
   end case ";"
  ;
/* 3.8.1 */
variant :
   when discrete_choice_list "=>"
      component_list
  ;
/* 3.8.1 */
discrete_choice_list : discrete_choice {"|" discrete_choice}
  ;
/* 3.8.1 */
discrete_choice : expression | discrete_range | others
  ;
/* 3.9.1 */
record_extension_part : with record_definition
  ;
/* 3.10 */
access_type_definition :
    access_to_object_definition
  | access_to_subprogram_definition
  ;
/* 3.10 */
access_to_object_definition :
    access [general_access_modifier] subtype_indication
  ;
/* 3.10 */
general_access_modifier : all | constant
  ;
/* 3.10 */
access_to_subprogram_definition :
    access [protected] procedure parameter_profile
  | access [protected] function  parameter_and_result_profile
  ;
/* 3.10 */
access_definition : access subtype_mark
  ;
/* 3.10.1 */
incomplete_type_declaration : type defining_identifier [discriminant_part] ";"
  ;
/* 3.11 */
declarative_part : {declarative_item}
  ;
/* 3.11 */
declarative_item :
    basic_declarative_item | _body
  ;
/* 3.11 */
basic_declarative_item :
    basic_declaration | representation_clause | use_clause
  ;
/* 3.11 */
_body : proper_body | body_stub
  ;
/* 3.11 */
proper_body :
    subprogram_body | package_body | task_body | protected_body
  ;
/* 4.1 */
name :
     direct_name                | explicit_dereference
   | indexed_component          | slice
   | selected_component         | attribute_reference
   | type_conversion            | function_call
   | character_literal
  ;
/* 4.1 */
direct_name : identifier | operator_symbol
  ;
/* 4.1 */
prefix : name | implicit_dereference
  ;
/* 4.1 */
explicit_dereference : name "." all
  ;
/* 4.1 */
implicit_dereference : name
  ;
/* 4.1.1 */
indexed_component : prefix "(" expression {"," expression} ")"
  ;
/* 4.1.2 */
slice : prefix"(" discrete_range ")"
  ;
/* 4.1.3 */
selected_component : prefix "." selector_name
  ;
/* 4.1.3 */
selector_name : identifier | character_literal | operator_symbol
  ;
/* 4.1.4 */
attribute_reference : prefix "'" attribute_designator
  ;
/* 4.1.4 */
attribute_designator :
    identifier["(" expression ")"]
  | Access | Delta | Digits
  ;
/* 4.1.4 */
range_attribute_reference : prefix "'" range_attribute_designator
  ;
/* 4.1.4 */
range_attribute_designator : Range["(" expression ")"]
  ;
/* 4.3 */
aggregate : record_aggregate | extension_aggregate | array_aggregate
  ;
/* 4.3.1 */
record_aggregate : "(" record_component_association_list ")"
  ;
/* 4.3.1 */
record_component_association_list :
    record_component_association {"," record_component_association}
  | null record
  ;
/* 4.3.1 */
record_component_association :
   [ component_choice_list "=>" ] expression
  ;
/* 4.3.1 */
component_choice_list :
    selector_name {"|" selector_name}
   | others
  ;
/* 4.3.2 */
extension_aggregate :
    "(" ancestor_part with record_component_association_list ")"
  ;
/* 4.3.2 */
ancestor_part : expression | subtype_mark
  ;
/* 4.3.3 */
array_aggregate :
  positional_array_aggregate | named_array_aggregate
  ;
/* 4.3.3 */
positional_array_aggregate :
    "(" expression"," expression {"," expression} ")"
  | "(" expression {"," expression} "," others "=>" expression ")"
  ;
/* 4.3.3 */
named_array_aggregate :
    "(" array_component_association {"," array_component_association} ")"
  ;
/* 4.3.3 */
array_component_association :
    discrete_choice_list "=>" expression
  ;
/* 4.4 */
expression :
     relation {and relation} | relation {and then relation}
   | relation {or relation}  | relation {or else relation}
   | relation {xor relation}
  ;
/* 4.4 */
relation :
     simple_expression [relational_operator simple_expression]
   | simple_expression [not] in _range
   | simple_expression [not] in subtype_mark
  ;
/* 4.4 */
simple_expression : [unary_adding_operator] term {binary_adding_operator term}
  ;
/* 4.4 */
term : factor {multiplying_operator factor}
  ;
/* 4.4 */
factor : primary ["**" primary] | abs primary | not primary
  ;
/* 4.4 */
primary :
   numeric_literal | null | string_literal | aggregate
 | name | qualified_expression | allocator | "(" expression ")"
  ;
/* 4.5 */
logical_operator             :  and | or  | xor
  ;
/* 4.5 */
relational_operator          :  "="   | "/="  | "<"   | "<=" | ">" | ">="
  ;
/* 4.5 */
binary_adding_operator       :  "+"   | "-"   | "&"
  ;
/* 4.5 */
unary_adding_operator        :  "+"   | "--"
  ;
/* 4.5 */
multiplying_operator         :  "*"   | "/"   | mod | rem
  ;
/* 4.5 */
highest_precedence_operator     :  "**"  | abs | not
  ;
/* 4.6 */
type_conversion :
    subtype_mark "(" expression ")"
  | subtype_mark "(" name ")"
  ;
/* 4.7 */
qualified_expression :
   subtype_mark "'" "(" expression ")" | subtype_mark "'" aggregate
  ;
/* 4.8 */
allocator :
   new subtype_indication | new qualified_expression
  ;
/* 5.1 */
sequence_of_statements : statement {statement}
  ;
/* 5.1 */
statement :
   {label} simple_statement | {label} compound_statement
  ;
/* 5.1 */
simple_statement : null_statement
   | assignment_statement              | exit_statement
   | goto_statement          | procedure_call_statement
   | return_statement        | entry_call_statement
   | requeue_statement       | delay_statement
   | abort_statement         | raise_statement
   | code_statement
  ;
/* 5.1 */
compound_statement :
     if_statement            | case_statement
   | loop_statement          | block_statement
   | accept_statement        | select_statement
  ;
/* 5.1 */
null_statement : null ";"
  ;
/* 5.1 */
label : "<<" statement_identifier ">>"
  ;
/* 5.1 */
statement_identifier : direct_name
  ;
/* 5.2 */
assignment_statement :
   variable_name ":=" expression ";"
  ;
/* 5.3 */
if_statement :
    if condition then
      sequence_of_statements
   {elsif condition then
      sequence_of_statements}
   [else
      sequence_of_statements]
    end if ";"
  ;
/* 5.3 */
condition : expression
  ;
/* 5.4 */
case_statement :
   case expression is
       case_statement_alternative
      {case_statement_alternative}
   end case ";"
  ;
/* 5.4 */
case_statement_alternative :
   when discrete_choice_list "=>"
      sequence_of_statements
  ;
/* 5.5 */
loop_statement :
   [statement_identifier ":"]
      [iteration_scheme] loop
         sequence_of_statements
       end loop [statement_identifier] ";"
  ;
/* 5.5 */
iteration_scheme : while condition
   | for loop_parameter_specification
  ;
/* 5.5 */
loop_parameter_specification :
   defining_identifier in [reverse] discrete_subtype_definition
  ;
/* 5.6 */
block_statement :
   [statement_identifier ":"]
       [declare
            declarative_part]
        begin
            handled_sequence_of_statements
        end [statement_identifier] ";"
  ;
/* 5.7 */
exit_statement :
   exit [loop_name] [when condition] ";"
  ;
/* 5.8 */
goto_statement : goto label_name ";"
  ;
/* 6.1 */
subprogram_declaration : subprogram_specification ";"
  ;
/* 6.1 */
abstract_subprogram_declaration : subprogram_specification is abstract ";"
  ;
/* 6.1 */
subprogram_specification :
     procedure defining_program_unit_name  parameter_profile
   | function defining_designator  parameter_and_result_profile
  ;
/* 6.1 */
designator : [parent_unit_name "." ] identifier | operator_symbol
  ;
/* 6.1 */
defining_designator : defining_program_unit_name | defining_operator_symbol
  ;
/* 6.1 */
defining_program_unit_name : [parent_unit_name "." ] defining_identifier
  ;
/* 6.1 */
operator_symbol : string_literal
  ;
/* 6.1 */
defining_operator_symbol : operator_symbol
  ;
/* 6.1 */
parameter_profile : [formal_part]
  ;
/* 6.1 */
parameter_and_result_profile : [formal_part] return subtype_mark
  ;
/* 6.1 */
formal_part :
   "(" parameter_specification { ";" parameter_specification} ")"
  ;
/* 6.1 */
parameter_specification :
    defining_identifier_list ":" mode  subtype_mark [":=" default_expression]
  | defining_identifier_list ":" access_definition [":=" default_expression]
  ;
/* 6.1 */
mode : [in] | in out | out
  ;
/* 6.3 */
subprogram_body :
    subprogram_specification is
       declarative_part
    begin
        handled_sequence_of_statements
    end [designator] ";"
  ;
/* 6.4 */
procedure_call_statement :
    procedure_name ";"
  | prefix actual_parameter_part ";"
  ;
/* 6.4 */
function_call :
    function_name
  | prefix actual_parameter_part
  ;
/* 6.4 */
actual_parameter_part :
    "(" parameter_association {"," parameter_association} ")"
  ;
/* 6.4 */
parameter_association :
   [selector_name "=>"] explicit_actual_parameter
  ;
/* 6.4 */
explicit_actual_parameter : expression | variable_name
  ;
/* 6.5 */
return_statement : return [expression] ";"
  ;
/* 7.1 */
package_declaration : package_specification ";"
  ;
/* 7.1 */
package_specification :
    package defining_program_unit_name is
      {basic_declarative_item}
   [private
      {basic_declarative_item}]
    end [[parent_unit_name "."] identifier]
  ;
/* 7.2 */
package_body :
    package body defining_program_unit_name is
       declarative_part
   [begin
        handled_sequence_of_statements]
    end [[parent_unit_name "."] identifier] ";"
  ;
/* 7.3 */
private_type_declaration :
   type defining_identifier [discriminant_part] is [[abstract] tagged]
   [limited] private ";" 
  ;
/* 7.3 */
private_extension_declaration :
   type defining_identifier [discriminant_part] is
     [abstract] new subtype_indication with private ";"
  ;
/* 8.4 */
use_clause : use_package_clause | use_type_clause
  ;
/* 8.4 */
use_package_clause : use package_name {"," package_name} ";"
  ;
/* 8.4 */
use_type_clause : use type subtype_mark {"," subtype_mark} ";"
  ;
/* 8.5 */
renaming_declaration :
      object_renaming_declaration
    | exception_renaming_declaration
    | package_renaming_declaration
    | subprogram_renaming_declaration
    | generic_renaming_declaration
  ;
/* 8.5.1 */
object_renaming_declaration : defining_identifier ":" subtype_mark renames name ";"
  ;
/* 8.5.2 */
exception_renaming_declaration : defining_identifier ":" exception renames name ";"
  ;
/* 8.5.3 */
package_renaming_declaration : package defining_program_unit_name renames name ";"
  ;
/* 8.5.4 */
subprogram_renaming_declaration : subprogram_specification renames  name ";"
  ;
/* 8.5.5 */
generic_renaming_declaration :
    generic package       defining_program_unit_name renames name ";"
  | generic procedure     defining_program_unit_name renames name ";"
  | generic function      defining_program_unit_name renames name ";"
  ;
/* 9.1 */
task_type_declaration :
   task type defining_identifier [known_discriminant_part] [is task_definition] ";"
  ;
/* 9.1 */
single_task_declaration :
   task defining_identifier [is task_definition] ";"
  ;
/* 9.1 */
task_definition :
     {task_item}
  [ private
     {task_item}]
  end [task_identifier]
  ;
/* 9.1 */
task_item : entry_declaration | representation_clause
  ;
/* 9.1 */
task_body :
   task body defining_identifier is
     declarative_part
   begin
     handled_sequence_of_statements
   end [task_identifier] ";"  
  ;
/* 9.4 */
protected_type_declaration :
  protected type defining_identifier [known_discriminant_part] is
  protected_definition ";" 
  ;
/* 9.4 */
single_protected_declaration :
  protected defining_identifier is protected_definition ";"
  ;
/* 9.4 */
protected_definition :
    { protected_operation_declaration }
[ private
    { protected_element_declaration } ]
  end [protected_identifier]
  ;
/* 9.4 */
protected_operation_declaration : subprogram_declaration
     | entry_declaration
     | representation_clause
  ;
/* 9.4 */
protected_element_declaration : protected_operation_declaration
     | component_declaration
  ;
/* 9.4 */
protected_body :
  protected body defining_identifier is
   { protected_operation_item }
  end [protected_identifier] ";"
  ;
/* 9.4 */
protected_operation_item : subprogram_declaration
     | subprogram_body
     | entry_body
     | representation_clause
  ;
/* 9.5.2 */
entry_declaration :
   entry defining_identifier ["(" discrete_subtype_definition ")"]
   parameter_profile ";" 
  ;
/* 9.5.2 */
accept_statement :
   accept direct_name ["(" entry_index ")"] parameter_profile [do
     handled_sequence_of_statements
   end [entry_identifier]] ";"
  ;
/* 9.5.2 */
entry_index : expression
  ;
/* 9.5.2 */
entry_body :
  entry defining_identifier  entry_body_formal_part  entry_barrier is
    declarative_part
  begin
    handled_sequence_of_statements
  end [entry_identifier] ";"
  ;
/* 9.5.2 */
entry_body_formal_part : ["(" entry_index_specification ")"] parameter_profile
  ;
/* 9.5.2 */
entry_barrier : when condition
  ;
/* 9.5.2 */
entry_index_specification : for defining_identifier in discrete_subtype_definition
  ;
/* 9.5.3 */
entry_call_statement : entry_name [actual_parameter_part] ";"
  ;
/* 9.5.4 */
requeue_statement : requeue entry_name [with abort] ";"
  ;
/* 9.6 */
delay_statement : delay_until_statement | delay_relative_statement
  ;
/* 9.6 */
delay_until_statement : delay until expression ";"
  ;
/* 9.6 */
delay_relative_statement : delay expression ";"
  ;
/* 9.7 */
select_statement :
   selective_accept
  | timed_entry_call
  | conditional_entry_call
  | asynchronous_select
  ;
/* 9.7.1 */
selective_accept :
  select
   [guard]
     select_alternative
{ or
   [guard]
     select_alternative }
[ else
   sequence_of_statements ]
  end select ";"
  ;
/* 9.7.1 */
guard : when condition "=>"
  ;
/* 9.7.1 */
select_alternative :
   accept_alternative
  | delay_alternative
  | terminate_alternative
  ;
/* 9.7.1 */
accept_alternative :
  accept_statement [sequence_of_statements]
  ;
/* 9.7.1 */
delay_alternative :
  delay_statement [sequence_of_statements]
  ;
/* 9.7.1 */
terminate_alternative : terminate ";"
  ;
/* 9.7.2 */
timed_entry_call :
  select
   entry_call_alternative
  or
   delay_alternative
  end select ";"
  ;
/* 9.7.2 */
entry_call_alternative :
  entry_call_statement [sequence_of_statements]
  ;
/* 9.7.3 */
conditional_entry_call :
  select
   entry_call_alternative
  else
   sequence_of_statements
  end select ";"
  ;
/* 9.7.4 */
asynchronous_select :
  select
   triggering_alternative
  then abort
   abortable_part
  end select ";"
  ;
/* 9.7.4 */
triggering_alternative : triggering_statement [sequence_of_statements]
  ;
/* 9.7.4 */
triggering_statement : entry_call_statement | delay_statement
  ;
/* 9.7.4 */
abortable_part : sequence_of_statements
  ;
/* 9.8 */
abort_statement : abort task_name {"," task_name} ";"
  ;
/* 10.1.1 */
compilation : {compilation_unit}
  ;
/* 10.1.1 */
compilation_unit :
    context_clause library_item
  | context_clause subunit
  ;
/* 10.1.1 */
library_item : [private] library_unit_declaration
  | library_unit_body
  | [private] library_unit_renaming_declaration
  ;
/* 10.1.1 */
library_unit_declaration :
     subprogram_declaration | package_declaration
   | generic_declaration  | generic_instantiation
  ;
/* 10.1.1 */
library_unit_renaming_declaration :
   package_renaming_declaration
 | generic_renaming_declaration
 | subprogram_renaming_declaration
  ;
/* 10.1.1 */
library_unit_body : subprogram_body | package_body
  ;
/* 10.1.1 */
parent_unit_name : name
  ;
/* 10.1.2 */
context_clause : {context_item}
  ;
/* 10.1.2 */
context_item : with_clause | use_clause
  ;
/* 10.1.2 */
with_clause : with library_unit_name {"," library_unit_name} ";"
  ;
/* 10.1.3 */
body_stub : subprogram_body_stub | package_body_stub | task_body_stub | protected_body_stub
  ;
/* 10.1.3 */
subprogram_body_stub : subprogram_specification is separate ";"
  ;
/* 10.1.3 */
package_body_stub : package body defining_identifier is separate ";"
  ;
/* 10.1.3 */
task_body_stub : task body defining_identifier is separate ";"
  ;
/* 10.1.3 */
protected_body_stub : protected body defining_identifier is separate ";"
  ;
/* 10.1.3 */
subunit : separate "(" parent_unit_name ")" proper_body
  ;
/* 11.1 */
exception_declaration : defining_identifier_list ":" exception ";"
  ;
/* 11.2 */
handled_sequence_of_statements :
     sequence_of_statements
  [exception
     exception_handler
    {exception_handler}]
  ;
/* 11.2 */
exception_handler :
  when [choice_parameter_specification ":" ] 
  exception_choice {"|" exception_choice} "=>" sequence_of_statements
  ;
/* 11.2 */
choice_parameter_specification : defining_identifier
  ;
/* 11.2 */
exception_choice : exception_name | others
  ;
/* 11.3 */
raise_statement : raise [exception_name] ";"
  ;
/* 12.1 */
generic_declaration : generic_subprogram_declaration | generic_package_declaration
  ;
/* 12.1 */
generic_subprogram_declaration :
     generic_formal_part  subprogram_specification ";"
  ;
/* 12.1 */
generic_package_declaration :
     generic_formal_part  package_specification ";"
  ;
/* 12.1 */
generic_formal_part : generic {generic_formal_parameter_declaration | use_clause}
  ;
/* 12.1 */
generic_formal_parameter_declaration :
      formal_object_declaration
    | formal_type_declaration
    | formal_subprogram_declaration
    | formal_package_declaration
  ;
/* 12.3 */
generic_instantiation :
     package defining_program_unit_name is
         new package_name [generic_actual_part] ";"
   | procedure defining_program_unit_name is
         new procedure_name [generic_actual_part] ";"
   | function defining_designator is
         new function_name [generic_actual_part] ";"
  ;
/* 12.3 */
generic_actual_part :
   "(" generic_association {"," generic_association} ")"
  ;
/* 12.3 */
generic_association :
   [selector_name "=>"] explicit_generic_actual_parameter
  ;
/* 12.3 */
explicit_generic_actual_parameter : expression | variable_name
   | procedure_name | function_name | entry_name | subtype_mark
   | package_name
  ;
/* 12.4 */
formal_object_declaration :
    defining_identifier_list ":" mode subtype_mark [":=" default_expression] ";"
  ;
/* 12.5 */
formal_type_declaration :
    type defining_identifier[discriminant_part] is formal_type_definition ";"
  ;
/* 12.5 */
formal_type_definition :
      formal_private_type_definition
    | formal_derived_type_definition
    | formal_discrete_type_definition
    | formal_signed_integer_type_definition
    | formal_modular_type_definition
    | formal_floating_point_definition
    | formal_ordinary_fixed_point_definition
    | formal_decimal_fixed_point_definition
    | formal_array_type_definition
    | formal_access_type_definition
  ;
/* 12.5.1 */
formal_private_type_definition : [[abstract] tagged] [limited] private
  ;
/* 12.5.1 */
formal_derived_type_definition : [abstract] new subtype_mark [with private]
  ;
/* 12.5.2 */
formal_discrete_type_definition : "(" "<>" ")"
  ;
/* 12.5.2 */
formal_signed_integer_type_definition : range "<>"
  ;
/* 12.5.2 */
formal_modular_type_definition : mod "<>"
  ;
/* 12.5.2 */
formal_floating_point_definition : digits "<>"
  ;
/* 12.5.2 */
formal_ordinary_fixed_point_definition : delta "<>"
  ;
/* 12.5.2 */
formal_decimal_fixed_point_definition : delta "<>" digits "<>"
  ;
/* 12.5.3 */
formal_array_type_definition : array_type_definition
  ;
/* 12.5.4 */
formal_access_type_definition : access_type_definition
  ;
/* 12.6 */
formal_subprogram_declaration : with subprogram_specification [is subprogram_default] ";"
  ;
/* 12.6 */
subprogram_default : default_name | "<>"
  ;
/* 12.6 */
default_name : name
  ;
/* 12.7 */
formal_package_declaration :
    with package defining_identifier is new package_name  formal_package_actual_part ";"
  ;
/* 12.7 */
formal_package_actual_part :
    "(" "<>" ")" | [generic_actual_part]
  ;
/* 13.1 */
representation_clause : attribute_definition_clause
      | enumeration_representation_clause
      | record_representation_clause
      | at_clause
  ;
/* 13.1 */
local_name : direct_name
      | direct_name "'" attribute_designator
      | library_unit_name
  ;
/* 13.3 */
attribute_definition_clause :
      for local_name "'" attribute_designator use expression ";"
    | for local_name "'" attribute_designator use name ";"
  ;
/* 13.4 */
enumeration_representation_clause :
    for first_subtype_local_name use enumeration_aggregate ";"
  ;
/* 13.4 */
enumeration_aggregate : array_aggregate
  ;
/* 13.5.1 */
record_representation_clause :
    for first_subtype_local_name use
      record [mod_clause]
        {component_clause}
      end record ";"
  ;
/* 13.5.1 */
component_clause :
    local_name at position range first_bit ".." last_bit ";"
  ;
/* 13.5.1 */
position : expression
  ;
/* 13.5.1 */
first_bit : static_simple_expression
  ;
/* 13.5.1 */
last_bit : static_simple_expression
  ;
/* 13.8 */
code_statement : qualified_expression ";"
  ;
/* 13.12 */
restriction : identifier
    | identifier "=>" expression
  ;
/* J.3: */
delta_constraint : delta expression [range_constraint]
  ;
/* J.7: */
at_clause : for direct_name use at expression ";"
  ;
/* J.8: */
mod_clause : at mod expression ";"
  ;

static_simple_expression : simple_expression 
  ;
variable_name : name 
  ;
loop_name : name
  ;
label_name
  : name
  ;
procedure_name
  : name
  ;
function_name
  : name
  ;
package_name
  : name
  ;
entry_name
  : name
  ;
task_name
  : name
  ;
library_unit_name
  : name
  ;
exception_name
  : name
  ;
task_identifier : identifier 
  ;
protected_identifier : identifier 
  ;
entry_identifier : identifier 
  ;
first_subtype_local_name : direct_name 
  ;

/*** From the page footer:
 *** Email comments, additions, corrections, gripes, kudos, etc. to:
 *** Magnus Kempe -- M.Kempe@ieee.org
 *** Copyright statement
 *** Page last generated: 95-03-12 ( non-terminal cross-references 
 *** contributed by Volker Elling 96-01-09 ")"
 ***/
