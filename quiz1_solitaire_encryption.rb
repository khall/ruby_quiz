# http://rubyquiz.com/quiz1.html
# Usage: ruby solitaire.rb encrypt "Message to be encrypted" "3, 25, 26, 28, 12, 49, 51, 4, 10, 36, 8, 22, 34, 39, 2, 23, 1, 47, 5, A, 17, 30, 48, 19, 45, 42, 46, 32, 21, B, 43, 35, 40, 37, 29, 44, 38, 14, 11, 16, 41, 15, 9, 24, 50, 7, 18, 33, 27, 52, 31, 13, 6, 20"
#        ruby filename.rb  action  "Quoted plain text"       "Quoted deck of 'cards' (1-52) with jokers ('A', 'B') for a key"

$deck = nil

# decrypt ciphertext
def decrypt
  cipher = ARGV[1]
  $deck = format_deck(ARGV[2])
  int_cipher = str_to_ints cipher
  keystream = keystream_sequence(cipher.gsub(/ /, '').length)
  int_keystream = str_to_ints keystream
  subbed_cipher = subtract(int_cipher, int_keystream)
  ints_to_str subbed_cipher
end

# encrypt plaintext
def encrypt
  input = format_input
  $deck = format_deck(ARGV[2])
  keystream = keystream_sequence(input.gsub(/ /, '').length)
  input = str_to_ints(input)
  keystream = str_to_ints(keystream)
  sum = add(input, keystream)
  ints_to_str sum
end

# pull input from the command line and format it
def format_input
  str = ARGV[1].gsub(/[^a-zA-Z]+/, '').upcase
  divided = []
  while str.length > 0
    word = str.slice(0, 5)
    str.slice!(0, 5)
    while word.length < 5
      word += "X"
    end
    divided << word
  end
  divided.join ' '
end

def format_deck(deck)
  deck.split(',').map do |n|
    n = n.strip
    if ['A', 'B'].include? n
      n
    else
      n.to_i
    end
  end
end

# convert chars to ints, preserve spaces
def str_to_ints(str)
  str.chars.map{|c| c == ' ' ? c : c.ord - 64 }
end

# convert ints to chars, preserve spaces
def ints_to_str(ints)
  ints.map{|c| c == ' ' ? c : (c + 64).chr }.join('')
end

# add similarly indexed integers, subtracting 26 if greater than 26
def add(arr1, arr2)
  sum_arr = []
  arr1.each_index do |i|
    if arr1[i] == ' '
      sum_arr << ' '
      next
    end
    sum = arr1[i] + arr2[i]
    sum_arr << (sum > 26 ? sum - 26 : sum)
  end
  sum_arr
end

# subtract similarly indexed integers (message - keystream), add 26 to message number if less than or equal to keystream
def subtract(message, keystream)
  sub_arr = []
  message.each_index do |i|
    if message[i] == ' '
      sub_arr << ' '
      next
    end
    if message[i] <= keystream[i]
      message[i] += 26
    end
    sub_arr << message[i] - keystream[i]
  end
  sub_arr
end

# creates an unkeyed deck of cards to use for our keystream
# def create_deck
#   $deck = Array.new(52) {|i| i + 1}
#   $deck << 'A'
#   $deck << 'B'
# end

# generate len characters
def keystream_sequence(len)
  str = ''
  i = 0
  while str.gsub(/ /, '').length < len do
    letter = keystream_letter
    str += ' ' if i % 5 == 0 && i != 0 && letter != ''
    i += 1 unless letter.empty?
    str += letter
  end
  str
end

# perform all steps for grabbing a single keystream letter
def keystream_letter
  move_joker_a
  move_joker_b
  triple_cut
  count_cut
  count_down
end

# moves joker 'A' down one card (wrap to front as if deck is circular)
def move_joker_a
  a_pos = $deck.find_index('A')
  if a_pos == 53
    $deck.delete 'A'
    $deck.insert 1, 'A'
  else
    temp = $deck[a_pos + 1]
    $deck[a_pos + 1] = $deck[a_pos]
    $deck[a_pos] = temp
  end
end

# move joker 'B' down two cards (wrap to front as if deck is circular)
def move_joker_b
  b_pos = $deck.find_index('B')
  $deck.delete('B')
  if b_pos == 51
    $deck << 'B'
  else
    $deck.insert (b_pos + 2) % 53, 'B'
  end
end

# all cards above the top joker move to below the bottom joker and vice versa. Remaining cards don't move
def triple_cut
  joker_a = $deck.find_index('A')
  joker_b = $deck.find_index('B')
  top_joker = [joker_a, joker_b].min
  bottom_joker = [joker_a, joker_b].max

  above_cards = $deck.slice(0, [0, top_joker].max)
  below_cards = $deck.slice([$deck.length - 1, bottom_joker + 1].min, $deck.length - bottom_joker - 1)
  middle_cards = $deck.slice(top_joker, bottom_joker - top_joker + 1)

  $deck = [below_cards, middle_cards, above_cards].flatten
end

# cut the bottom card's value in cards off the top of the deck and reinsert them just above the bottom card
def count_cut
  bottom_card = $deck.last
  bottom_card = 53 if ['A', 'B'].include? bottom_card # joker's value is 53
  $deck = [$deck.slice(bottom_card, $deck.length - bottom_card - 1), $deck.slice(0, bottom_card), $deck.last].flatten
end

# Convert the top card to it's value and count down that many cards from the top of the deck, with the top card itself
# being card number one. Look at the card immediately after your count and convert it to a letter.
# This is the next letter in the keystream. If the output card is a joker, no letter is generated this sequence.
# This step does not alter the deck.
def count_down
  top_card = $deck.first
  top_card = 53 if ['A', 'B'].include? top_card
  if ['A', 'B'].include? $deck[top_card]
    return ''
  end

  if ($deck[top_card] > 26)
    ($deck[top_card] + 38).chr
  else
    ($deck[top_card] + 64).chr
  end
end

def control
  if ARGV.length != 3
    raise 'Usage: ruby solitaire.rb encrypt "Message to be encrypted" "3, 25, 26, 28, 12, 49, 51, 4, 10, 36, 8, 22, 34, 39, 2, 23, 1, 47, 5, A, 17, 30, 48, 19, 45, 42, 46, 32, 21, B, 43, 35, 40, 37, 29, 44, 38, 14, 11, 16, 41, 15, 9, 24, 50, 7, 18, 33, 27, 52, 31, 13, 6, 20"'
  end
  if ARGV[0] == 'encrypt'
    puts encrypt
  elsif ARGV[0] == 'decrypt'
    puts decrypt
  else
    raise "unknown"
  end
end

control