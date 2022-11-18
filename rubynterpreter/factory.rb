require_relative 'expressions'
require_relative 'statements'

class StatementCreator
  @instance = new

  private_class_method :new

  def self.instance
    @instance
  end

  def create(key, value)
    case key
    when "Assignment"
      Assignment.new(value[0], value[1])
    when "Block1"
      Block.new(value[0])
    when "Block2"
      Block.new()
    when "IfThenElse1"
      IfThenElse.new(value[0], value[1], value[2])
    when "IfThenElse2"
      IfThenElse.new(value[0], value[1], nil)
    when "While"
      WhileDo.new(val[0], val[1])
    when "Print"
      PrintStmt.new(val[0])
    end
  end
end

class ExpressionCreator
  @instance = new

  private_class_method :new

  def self.instance
    @instance
  end

  def create(key, value)
    case key
    when "ID"
      VariableExp.new(value[0])
    when "NUM"
      Numeral.new(value[0])
    when "-"
      Minus.new(value[0])
    when "!"
      Negation.new(value[0])
    when "+"
      Addition.new(value[0]+value[1])
    when "*"
      Multiplication.new(value[0]+value[1])
    when "/"
      Division.new(value[0]+value[1])
    when "=="
      ComparisonEqual.new(value[0]+value[1])
    when "!="
      ComparisonDifferent.new(value[0]+value[1])
    when "<"
      ComparisonLessThan.new(value[0]+value[1])
    when "<="
      ComparisonLessThanOrEqual.new(value[0]+value[1])
    when ">"
      ComparisonGreaterThan.new(value[0]+value[1])
    when ">="
      ComparisonGreaterThanOrEqual.new(value[0]+value[1])
    when "&&"
      LogicalAnd.new(value[0]+value[1])
    when "||"
      LogicalOr.new(value[0]+value[1])
    end  
  end
end