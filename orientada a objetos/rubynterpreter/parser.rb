#
# DO NOT MODIFY!!!!
# This file is automatically generated by Racc 1.4.14
# from Racc grammer file "".
#

require 'racc/parser.rb'


require 'strscan'
require_relative 'expressions'
require_relative 'statements'

class Lexer
  NUMBER = /\d+(\.\d+)?/
  BOOL = /true|false/
  KEYWORDS = /if|else|while|print/
  ID = /[a-zA-Z_]\w*/
  OPERS = /[-+*\/(){};]|[!=<>]=?|&&|\|\|/
  IGNORE = /(\/\/.*(\n|$)|\s)+/
  
  def initialize(input)
    @ss = StringScanner.new(input)
  end

  def next_token
    @ss.scan(IGNORE)
    return if @ss.eos?
    case
    when text = @ss.scan(NUMBER) then [:NUM, text.to_f]
    when text = @ss.scan(BOOL) then [:BOOL, text == "true"]
    when text = @ss.scan(KEYWORDS) then [text, text]
    when text = @ss.scan(ID) then [:ID, text]
    when text = @ss.scan(OPERS) then [text, text]
    else
      throw "Unexpected character #{@ss.getch}!"
    end
  end
end

class Parser < Racc::Parser

module_eval(<<'...end parser.racc/module_eval...', 'parser.racc', 115)

def next_token
  @lexer.next_token
end

def tokenize_string(input)
  @lexer = Lexer.new(input)
  tokens = []
  while token = @lexer.next_token
    tokens << token
  end
  tokens
end

def parse_string(input)
  @lexer = Lexer.new(input)
  do_parse
end


...end parser.racc/module_eval...
##### State transition tables begin ###

racc_action_table = [
    26,    25,    19,    20,    21,    22,    23,    24,    17,    18,
    15,    16,    26,    25,    19,    20,    21,    22,    23,    24,
    14,    58,    26,    25,    19,    20,    21,    22,    23,    24,
    17,    18,    15,    16,    26,    25,    19,    20,    21,    22,
    23,    24,    27,    60,    26,    25,    19,    20,    21,    22,
    23,    24,    17,    18,    15,    16,    26,    25,   -30,   -30,
   -30,   -30,   -30,   -30,    32,    61,    26,    25,    19,    20,
    21,    22,    23,    24,    17,    18,    15,    16,    26,    25,
   -30,   -30,   -30,   -30,   -30,   -30,    33,    62,    26,    25,
    19,    20,    21,    22,    23,    24,    17,    18,    15,    16,
    11,    34,    12,     4,    59,    39,     5,    27,     6,    13,
    26,    65,     7,     8,     9,    10,    11,    66,    12,    35,
    11,   nil,    12,    35,    11,    13,    12,    35,   nil,    13,
     9,    10,   nil,    13,     9,    10,   nil,   nil,     9,    10,
    11,   nil,    12,    35,    11,   nil,    12,    35,    11,    13,
    12,    35,   nil,    13,     9,    10,   nil,    13,     9,    10,
   nil,   nil,     9,    10,    11,   nil,    12,    35,    11,   nil,
    12,    35,    11,    13,    12,    35,   nil,    13,     9,    10,
   nil,    13,     9,    10,   nil,   nil,     9,    10,    11,   nil,
    12,    35,    11,   nil,    12,    35,    11,    13,    12,    35,
   nil,    13,     9,    10,   nil,    13,     9,    10,   nil,   nil,
     9,    10,    11,   nil,    12,    35,    11,   nil,    12,    35,
    11,    13,    12,    35,   nil,    13,     9,    10,   nil,    13,
     9,    10,   nil,   nil,     9,    10,    11,   nil,    12,    35,
    11,   nil,    12,    35,    11,    13,    12,    35,   nil,    13,
     9,    10,   nil,    13,     9,    10,   nil,   nil,     9,    10,
    11,    28,    12,    35,     5,    30,     6,   nil,   nil,    13,
     7,     8,   nil,   nil,     9,    10,    26,    25,    19,    20,
    21,    22,    23,    24,    17,    18,    15,    16,    26,    25,
    19,    20,    21,    22,    23,    24,    17,    18,    15,    16,
    26,    25,    19,    20,    21,    22,    23,    24,    17,    18,
    15,    16,    28,   nil,   nil,     5,    53,     6,   nil,   nil,
    28,     7,     8,     5,   nil,     6,   nil,   nil,    28,     7,
     8,     5,   nil,     6,   nil,   nil,    28,     7,     8,     5,
   nil,     6,   nil,   nil,   nil,     7,     8,    26,    25,    19,
    20,    21,    22,    23,    24,    17,    18,    26,    25,    19,
    20,    21,    22,    23,    24,    17,    18,    26,    25,   -30,
   -30,   -30,   -30,   -30,   -30,    26,    25,   -30,   -30,   -30,
   -30,   -30,   -30,    26,    25,   -30,   -30,   -30,   -30,   -30,
   -30,    26,    25,   -30,   -30,   -30,   -30,   -30,   -30 ]

racc_action_check = [
    38,    38,    38,    38,    38,    38,    38,    38,    38,    38,
    38,    38,    42,    42,    42,    42,    42,    42,    42,    42,
     1,    38,    55,    55,    55,    55,    55,    55,    55,    55,
    55,    55,    55,    55,    43,    43,    43,    43,    43,    43,
    43,    43,     4,    55,    56,    56,    56,    56,    56,    56,
    56,    56,    56,    56,    56,    56,    44,    44,    44,    44,
    44,    44,    44,    44,     6,    56,    57,    57,    57,    57,
    57,    57,    57,    57,    57,    57,    57,    57,    45,    45,
    45,    45,    45,    45,    45,    45,     7,    57,    52,    52,
    52,    52,    52,    52,    52,    52,    52,    52,    52,    52,
     0,     8,     0,     0,    52,    14,     0,    28,     0,     0,
    50,    62,     0,     0,     0,     0,    11,    63,    11,    11,
    12,   nil,    12,    12,    13,    11,    13,    13,   nil,    12,
    11,    11,   nil,    13,    12,    12,   nil,   nil,    13,    13,
    15,   nil,    15,    15,    16,   nil,    16,    16,    17,    15,
    17,    17,   nil,    16,    15,    15,   nil,    17,    16,    16,
   nil,   nil,    17,    17,    18,   nil,    18,    18,    19,   nil,
    19,    19,    20,    18,    20,    20,   nil,    19,    18,    18,
   nil,    20,    19,    19,   nil,   nil,    20,    20,    21,   nil,
    21,    21,    22,   nil,    22,    22,    23,    21,    23,    23,
   nil,    22,    21,    21,   nil,    23,    22,    22,   nil,   nil,
    23,    23,    24,   nil,    24,    24,    25,   nil,    25,    25,
    26,    24,    26,    26,   nil,    25,    24,    24,   nil,    26,
    25,    25,   nil,   nil,    26,    26,    27,   nil,    27,    27,
    32,   nil,    32,    32,    33,    27,    33,    33,   nil,    32,
    27,    27,   nil,    33,    32,    32,   nil,   nil,    33,    33,
    34,     5,    34,    34,     5,     5,     5,   nil,   nil,    34,
     5,     5,   nil,   nil,    34,    34,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,    36,    36,
    36,    36,    36,    36,    36,    36,    36,    36,    36,    36,
    37,    37,    37,    37,    37,    37,    37,    37,    37,    37,
    37,    37,    29,   nil,   nil,    29,    29,    29,   nil,   nil,
    60,    29,    29,    60,   nil,    60,   nil,   nil,    61,    60,
    60,    61,   nil,    61,   nil,   nil,    66,    61,    61,    66,
   nil,    66,   nil,   nil,   nil,    66,    66,    40,    40,    40,
    40,    40,    40,    40,    40,    40,    40,    41,    41,    41,
    41,    41,    41,    41,    41,    41,    41,    46,    46,    46,
    46,    46,    46,    46,    46,    47,    47,    47,    47,    47,
    47,    47,    47,    48,    48,    48,    48,    48,    48,    48,
    48,    49,    49,    49,    49,    49,    49,    49,    49 ]

racc_action_pointer = [
    87,    20,   nil,   274,    25,   245,    42,    64,    79,   nil,
   nil,   103,   107,   111,   105,   127,   131,   135,   151,   155,
   159,   175,   179,   183,   199,   203,   207,   223,    90,   296,
   nil,   nil,   227,   231,   247,   nil,   286,   298,    -2,   nil,
   345,   355,    10,    32,    54,    76,   365,   373,   381,   389,
   108,   nil,    86,   nil,   nil,    20,    42,    64,   nil,   nil,
   304,   312,    93,    93,   nil,   nil,   320,   nil ]

racc_action_default = [
   -30,   -30,    -1,    -2,   -12,   -30,   -30,   -30,   -30,   -13,
   -14,   -30,   -30,   -30,   -30,   -30,   -30,   -30,   -30,   -30,
   -30,   -30,   -30,   -30,   -30,   -30,   -30,   -30,   -30,   -30,
    -5,   -10,   -30,   -30,   -30,   -12,   -15,   -16,   -30,    68,
   -17,   -18,   -19,   -20,   -21,   -22,   -23,   -24,   -25,   -26,
   -27,   -28,   -30,    -4,   -11,   -30,   -30,   -30,   -29,    -3,
   -30,   -30,   -30,    -7,    -8,    -9,   -30,    -6 ]

racc_goto_table = [
     2,     3,     1,    29,   nil,    31,   nil,   nil,   nil,   nil,
   nil,   nil,    36,    37,    38,   nil,    40,    41,    42,    43,
    44,    45,    46,    47,    48,    49,    50,    51,    52,    54,
   nil,   nil,   nil,    55,    56,    57,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,
    63,    64,   nil,   nil,   nil,   nil,    67 ]

racc_goto_check = [
     2,     3,     1,     4,   nil,     2,   nil,   nil,   nil,   nil,
   nil,   nil,     3,     3,     3,   nil,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     2,
   nil,   nil,   nil,     3,     3,     3,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,
     2,     2,   nil,   nil,   nil,   nil,     2 ]

racc_goto_pointer = [
   nil,     2,     0,     1,    -2 ]

racc_goto_default = [
   nil,   nil,   nil,   nil,   nil ]

racc_reduce_table = [
  0, 0, :racc_error,
  1, 30, :_reduce_1,
  1, 30, :_reduce_2,
  4, 31, :_reduce_3,
  3, 31, :_reduce_4,
  2, 31, :_reduce_5,
  7, 31, :_reduce_6,
  5, 31, :_reduce_7,
  5, 31, :_reduce_8,
  5, 31, :_reduce_9,
  1, 33, :_reduce_10,
  2, 33, :_reduce_11,
  1, 32, :_reduce_12,
  1, 32, :_reduce_13,
  1, 32, :_reduce_14,
  2, 32, :_reduce_15,
  2, 32, :_reduce_16,
  3, 32, :_reduce_17,
  3, 32, :_reduce_18,
  3, 32, :_reduce_19,
  3, 32, :_reduce_20,
  3, 32, :_reduce_21,
  3, 32, :_reduce_22,
  3, 32, :_reduce_23,
  3, 32, :_reduce_24,
  3, 32, :_reduce_25,
  3, 32, :_reduce_26,
  3, 32, :_reduce_27,
  3, 32, :_reduce_28,
  3, 32, :_reduce_29 ]

racc_reduce_n = 30

racc_shift_n = 68

racc_token_table = {
  false => 0,
  :error => 1,
  "||" => 2,
  "&&" => 3,
  "==" => 4,
  "!=" => 5,
  "<" => 6,
  "<=" => 7,
  ">" => 8,
  ">=" => 9,
  "*" => 10,
  "/" => 11,
  "+" => 12,
  "-" => 13,
  :UMINUS => 14,
  "!" => 15,
  :ID => 16,
  "=" => 17,
  ";" => 18,
  "{" => 19,
  "}" => 20,
  "if" => 21,
  "(" => 22,
  ")" => 23,
  "else" => 24,
  "while" => 25,
  "print" => 26,
  :NUM => 27,
  :BOOL => 28 }

racc_nt_base = 29

racc_use_result_var = false

Racc_arg = [
  racc_action_table,
  racc_action_check,
  racc_action_default,
  racc_action_pointer,
  racc_goto_table,
  racc_goto_check,
  racc_goto_default,
  racc_goto_pointer,
  racc_nt_base,
  racc_reduce_table,
  racc_token_table,
  racc_shift_n,
  racc_reduce_n,
  racc_use_result_var ]

Racc_token_to_s_table = [
  "$end",
  "error",
  "\"||\"",
  "\"&&\"",
  "\"==\"",
  "\"!=\"",
  "\"<\"",
  "\"<=\"",
  "\">\"",
  "\">=\"",
  "\"*\"",
  "\"/\"",
  "\"+\"",
  "\"-\"",
  "UMINUS",
  "\"!\"",
  "ID",
  "\"=\"",
  "\";\"",
  "\"{\"",
  "\"}\"",
  "\"if\"",
  "\"(\"",
  "\")\"",
  "\"else\"",
  "\"while\"",
  "\"print\"",
  "NUM",
  "BOOL",
  "$start",
  "target",
  "stmt",
  "exp",
  "stmts" ]

Racc_debug_parser = false

##### State transition tables end #####

# reduce 0 omitted

module_eval(<<'.,.,', 'parser.racc', 17)
  def _reduce_1(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 18)
  def _reduce_2(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 22)
  def _reduce_3(val, _values)
     Assignment.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 24)
  def _reduce_4(val, _values)
     Block.new(val[1]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 26)
  def _reduce_5(val, _values)
     Block.new() 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 28)
  def _reduce_6(val, _values)
     IfThenElse.new(val[2], val[4], val[6]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 30)
  def _reduce_7(val, _values)
     IfThenElse.new(val[2], val[4], nil) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 32)
  def _reduce_8(val, _values)
     WhileDo.new(val[2], val[4]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 34)
  def _reduce_9(val, _values)
     PrintStmt.new(val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 37)
  def _reduce_10(val, _values)
     [val[0]] 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 39)
  def _reduce_11(val, _values)
     val[0] << val[1] 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 42)
  def _reduce_12(val, _values)
     VariableExp.new(val[0]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 44)
  def _reduce_13(val, _values)
     Numeral.new(val[0]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 46)
  def _reduce_14(val, _values)
     TruthValue.new(val[0]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 48)
  def _reduce_15(val, _values)
     Minus.new(val[1]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 50)
  def _reduce_16(val, _values)
     Negation.new(val[1]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 52)
  def _reduce_17(val, _values)
     Addition.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 54)
  def _reduce_18(val, _values)
     Subtraction.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 56)
  def _reduce_19(val, _values)
     Multiplication.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 58)
  def _reduce_20(val, _values)
     Division.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 60)
  def _reduce_21(val, _values)
     ComparisonEqual.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 62)
  def _reduce_22(val, _values)
     ComparisonDifferent.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 64)
  def _reduce_23(val, _values)
     ComparisonLessThan.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 66)
  def _reduce_24(val, _values)
     ComparisonLessThanOrEqual.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 68)
  def _reduce_25(val, _values)
     ComparisonGreaterThan.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 70)
  def _reduce_26(val, _values)
     ComparisonGreaterThanOrEqual.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 72)
  def _reduce_27(val, _values)
     LogicalAnd.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 74)
  def _reduce_28(val, _values)
     LogicalOr.new(val[0], val[2]) 
  end
.,.,

module_eval(<<'.,.,', 'parser.racc', 76)
  def _reduce_29(val, _values)
     val[1] 
  end
.,.,

def _reduce_none(val, _values)
  val[0]
end

end   # class Parser

