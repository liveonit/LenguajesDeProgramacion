def tossUp
    array = []
    new_line = gets.chomp
    if !new_line.empty?
        array << new_line
        new_line = gets.chomp
    end
    puts array.sample
end
class Card
    attr_reader :suit
    attr_reader :rank
  
    def initialize(suit, rank)
      @suit = suit
      @rank = rank
    end
  end
  
  class Deck
    attr_reader :cards
  
    def initialize(cards)
      @cards = cards
    end
  
    def shuffle()
      cards.shuffle() #cards.shuffle!
      return self
    end
  
    def deal(n)
      return Deck.new(cards.shift(n))
    end
  
    def self.full()
      cards = []
      ["Espadas", "Bastos", "Oros", "Copas"].each do |suit|
        [1..12].each {|rank| cards << Card.new(suit, rank) }
      end
      return Deck.new(cards)
    end
  
  end
a = Card.new("espada",1)
b = Card.new("copas",2)
puts a.rank
puts a.suit

deck = Deck.new([a,b])
puts "#{deck.shuffle.cards}"

