require_relative 'expressions'
require_relative 'statements'
require_relative 'parser'
require "test/unit"
 
class TestSimpleNumber < Test::Unit::TestCase
 
    def test_numeral
        assert_equal(2.0, Numeral.new('2').evaluate )
    end

    def test_minus
        assert_equal(-2.0, Minus.new(Numeral.new('2')).evaluate )
    end
    
    def test_addition
        assert_equal(4.0, Addition.new(Numeral.new('2'), Numeral.new('2')).evaluate )
    end
 
    def test_substraction
        assert_equal(2.0, Subtraction.new(Numeral.new('4'), Numeral.new('2')).evaluate )
    end

    def test_multiplication
        assert_equal(8.0, Multiplication.new(Numeral.new('4'), Numeral.new('2')).evaluate )
    end

    def test_division
        assert_equal(2.0, Division.new(Numeral.new('4'), Numeral.new('2')).evaluate )
    end

    def test_eq
        assert_equal(true, ComparisonEqual.new(Numeral.new('4'), Numeral.new('4')).evaluate )
        assert_equal(false, ComparisonEqual.new(Numeral.new('4'), Numeral.new('5')).evaluate )
    end

    def test_diff
        assert_equal(false, ComparisonDifferent.new(Numeral.new('4'), Numeral.new('4')).evaluate )
        assert_equal(true, ComparisonDifferent.new(Numeral.new('4'), Numeral.new('5')).evaluate )
    end

    def test_less
        assert_equal(false, ComparisonLessThan.new(Numeral.new('4'), Numeral.new('4')).evaluate )
        assert_equal(true, ComparisonLessThan.new(Numeral.new('4'), Numeral.new('5')).evaluate )
    end

    def test_less_or_eq
        assert_equal(false, ComparisonLessThanOrEqual.new(Numeral.new('4'), Numeral.new('3')).evaluate )
        assert_equal(true, ComparisonLessThanOrEqual.new(Numeral.new('4'), Numeral.new('4')).evaluate )
        assert_equal(true, ComparisonLessThanOrEqual.new(Numeral.new('4'), Numeral.new('5')).evaluate )
    end

    def test_greater
        assert_equal(true, ComparisonGreaterThan.new(Numeral.new('4'), Numeral.new('3')).evaluate )
        assert_equal(false, ComparisonGreaterThan.new(Numeral.new('4'), Numeral.new('4')).evaluate )
        assert_equal(false, ComparisonGreaterThan.new(Numeral.new('4'), Numeral.new('5')).evaluate )
    end

    def test_greater_or_eq
        assert_equal(true, ComparisonGreaterThanOrEqual.new(Numeral.new('4'), Numeral.new('3')).evaluate )
        assert_equal(true, ComparisonGreaterThanOrEqual.new(Numeral.new('4'), Numeral.new('4')).evaluate )
        assert_equal(false, ComparisonGreaterThanOrEqual.new(Numeral.new('4'), Numeral.new('5')).evaluate )
    end

    def test_truth
        assert_equal(true, TruthValue.new(true).evaluate )
        assert_equal(false, TruthValue.new(false).evaluate )
    end

    def test_negation
        assert_equal(false, Negation.new(TruthValue.new(true)).evaluate )
        assert_equal(true, Negation.new(TruthValue.new(false)).evaluate )
    end

    def test_and
        assert_equal(true, LogicalAnd.new(TruthValue.new(true), TruthValue.new(true)).evaluate )
        assert_equal(false, LogicalAnd.new(TruthValue.new(false), TruthValue.new(true)).evaluate )
    end

    def test_or
        assert_equal(true, LogicalOr.new(TruthValue.new(true), TruthValue.new(true)).evaluate )
        assert_equal(true, LogicalOr.new(TruthValue.new(false), TruthValue.new(true)).evaluate )
    end

    def test_numerical_expression
        parser = Parser.new
        ast = parser.parse_string('1 - 2 * 3 + 6 / 2')
        assert_equal('((1.0-(2.0*3.0))+(6.0/2.0))', ast.unparse )
        assert_equal(-2.0, ast.evaluate )
    end


    def test_boolean_expression
        parser = Parser.new
        ast = parser.parse_string('!true || (!false && true) && 5 < 3')
        puts ast.unparse
        puts ast.evaluate
        assert_equal('((!true) || (((!false) && true ) && (5.0 < 3.0) ) )', ast.unparse )
        assert_equal(false, ast.evaluate )
    end


    def test_boolean_expression
        parser = Parser.new
        ast = parser.parse_string('!true || (!false && true) && 5 < 3')
        puts ast.unparse
        puts ast.evaluate
        assert_equal('((!true) || (((!false) && true ) && (5.0 < 3.0) ) )', ast.unparse )
        assert_equal(false, ast.evaluate )
    end

    # def test_assignation_statement
    #     parser = Parser.new
    #     ast = parser.parse_string('!true || (!false && true) && 5 < 3')
    #     puts ast.unparse
    #     puts ast.evaluate
    #     assert_equal('((!true) || (((!false) && true ) && (5.0 < 3.0) ) )', ast.unparse )
    #     assert_equal(false, ast.evaluate )
    # end

    # def test_if_statement
    #     parser = Parser.new
    #     ast = parser.parse_string('!true || (!false && true) && 5 < 3')
    #     puts ast.unparse
    #     puts ast.evaluate
    #     assert_equal('((!true) || (((!false) && true ) && (5.0 < 3.0) ) )', ast.unparse )
    #     assert_equal(false, ast.evaluate )
    # end

    # def test_while_statement
    #     parser = Parser.new
    #     ast = parser.parse_string('!true || (!false && true) && 5 < 3')
    #     puts ast.unparse
    #     puts ast.evaluate
    #     assert_equal('((!true) || (((!false) && true ) && (5.0 < 3.0) ) )', ast.unparse )
    #     assert_equal(false, ast.evaluate )
    # end


end


