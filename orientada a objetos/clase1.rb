#!/usr/bin/env ruby

class MegaReader
    def read()
        array = Array.new
        
        while line = gets.chomp
            if line.empty?
                break
            end
            array.push(line)
        end

        puts array.sample
    end
end

class Card  
    # Create the object
    def initialize(suit, rank)
      @suit = suit
      @rank = rank
    end

    attr_reader :suit
    attr_reader :rank
end

class Deck
    attr_reader :cards
    attr_reader :full

    # Create the object
    def initialize(cards)
      @cards = cards
    end

    def shuffle()
        @cards.shuffle
    end

    def deal(n) 
        arr = @cards.first(n)
        @cards = @cards.drop(n)
        Deck.new(arr)
    end

    def self.full()
        cards = []
        ["Espadas", "Bastos", "Oros", "Copas"].each do |suit|
          [1..12].each {|rank| cards << Card.new(suit, rank) }
        end
        return Deck.new(cards)
    end

    #Agregar el método peep que retorna la primera carta del mazo.
    def peep()
        cards[0]
    end

    #Agregar el método suitCount que dado un palo (o suit) retorna la cantidad de cartas en el mazo con ese palo.
    def suitCount(suit)
        cards.select  {|card| card.suit == suit }.length
    end
end

if __FILE__ == $0
    #mr = MegaReader.new
    #mr.read

    espadilla = Card.new("Espadas", 1)
    ancho = Card.new("Espadas", 7)

    borracho = Card.new("Copas", 12)

    deck = Deck.new([borracho, espadilla, ancho])
    puts ""
    puts "Numero de cartas de espada: #{deck.suitCount("Espadas")}"
    first = deck.peep()
    puts "Primera carta del mazo deberia ser 12 copas:#{first.rank} #{first.suit}"

end