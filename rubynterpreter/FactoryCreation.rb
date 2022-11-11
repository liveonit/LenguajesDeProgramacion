require_relative 'expressions'
require_relative 'statements'

class Creator

  def factory_method
    raise NotImplementedError, "#{self.class} has not implemented method '#{__method__}'"
  end

  def some_operation
    product = factory_method
    result = "Creator: The same creator's code has just worked with #{product.operation}"
    result
  end
end

class StatementCreator < Creator

  def factory_method(key, value)
    ConcreteProduct1.new
  end
end

class ExpressionCreator < Creator
  def factory_method(key, value)
    ConcreteProduct2.new
  end
end