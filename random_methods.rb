def quick_reverse(str)
  raise "Not a string" unless str.class == String
  str.reverse
end

def reverse(str)
  raise "Note a string" unless str.class == String
  len = str.length - 1
  reversed = Array.new(len)
  str.chars.each do |char|
    # Hello => olleH
    reversed[len] = char
    len -= 1
  end
  reversed.join('')
end

def reverse2(str)
  raise "Not a string" unless str.class == String
  len = str.length - 1
  i = 0
  while i < (len / 2.0 + 0.5).floor
    temp = str[i]
    str[i] = str[len - i]
    str[len - i] = temp
    i += 1
  end
  str
end

def binary_search(needle, haystack)
  raise "haystack is not an array" unless haystack.class == Array
  haystack.sort
  previous_i = 0
  while true
    i = haystack.length / 2
    return previous_i + i if needle == haystack[i]
    return -1 if haystack.length <= 1
    if needle < haystack[i]
      haystack = haystack[0..i-1]
    else
      haystack = haystack[i+1..haystack.length-1]
      previous_i = i + 1
    end
  end
end

def bs(needle, haystack, offset = 0)
  raise "haystack is not an array" unless haystack.class == Array
  i = haystack.length / 2
  if haystack[i] == needle
    return i + offset
  elsif haystack.length <= 1
    return -1
  elsif needle < haystack[i]
    bs needle, haystack[0..i-1], offset
  else
    offset += i + 1
    bs needle, haystack[i+1..haystack.length - 1], offset
  end
end

class Node
  @data = nil
  @right = nil
  @left = nil

  def initialize(data)
    @data = data
  end

  def right
    @right
  end

  def right=(r)
    @right = r
  end

  def left
    @left
  end

  def left=(l)
    @left = l
  end

  def data
    @data
  end

  def delete
    if self.left.nil?
      self.right.left = nil unless self.right.nil?
    else
      self.right.left = self.left
    end

    if self.right.nil?
      self.left.right = nil unless self.left.nil?
    else
      self.left.right = self.right
    end
    self.right = nil
    self.left = nil
    @data = nil
  end

  def insert_after(node)
    node.left = self
    node.right = right
    self.right.left = node unless right.nil?
    self.right = node
  end

  def has_cycle?(link = nil)
    link = self if link.nil?
    return false if self.right.nil?
    return true if self.right == link
    self.right.has_cycle? link
  end

  def middle(cur = nil, mid = nil, counter = 0)
    return nil if self.has_cycle?
    cur = self if cur.nil?
    mid = self if mid.nil?
    if cur.right.nil?
      return mid
    end
    prev_mid_count = counter / 2
    counter += 1
    if counter / 2 != prev_mid_count
      mid = mid.right
    end
    middle cur.right, mid, counter
  end
end

def reverse_words(str, new_str = '')
  broken = str.split(' ')
  new_str += broken.last + ' '
  return new_str if broken.length < 2
  broken = broken[0..broken.length - 2]
  reverse_words broken.join(' '), new_str
end

def reverse_words_in_place(str, pos = 0)
  broken = str.split(' ')
  return str if pos >= broken.length / 2
  temp = broken[pos]
  rear_pos = broken.length - 1 - pos
  broken[pos] = broken[rear_pos]
  broken[rear_pos] = temp
  reverse_words_in_place broken.join(' '), pos + 1
end

def strip_whitespace(str, pos = 0)
  return str if pos > str.length - 1
  if str[pos] =~ /\s/
    first = pos == 0 ? '' : str[0..pos-1]
    second = pos == str.length - 1 ? '' : str[pos+1..str.length-1]
    str = first + second
  end
  strip_whitespace str, pos + 1
end

def remove_duplicates(str, new_str = '', pos = 0)
  return new_str if pos >= str.length
  new_str += str[pos] unless new_str.include? str[pos]
  remove_duplicates str, new_str, pos+1
end

def find_first_unique(str)
  chars = []
  str.each_char do |c|
    if chars.include?(c)
      chars.delete c
    else
      chars << c
    end
  end
  chars.first
end

def find_duplicate(arr)
  # array has all integers between 1 and 1_000_000, one is a duplicate, return duplicate value
  arr.sort!
  arr.each_index do |i|
    return arr[i] if arr[i] == arr[i+1]
  end
  0
end

def find_missing(arr)
  # array has all integers between 1 and 1_000_000, one is missing, return missing value
  arr.sort!
  arr.each_index do |i|
    return arr[i] + 1 if arr[i] != arr[i+1] - 1
  end
  0
end

def largest_contiguous_sum(arr)
  old_sum = nil
  new_sum = nil
  highest = nil
  arr.each_index do |i|
    if old_sum.nil?
      old_sum = arr[i]
    else
      old_sum += arr[i]
    end
    new_sum = arr[i]
    if new_sum > old_sum
      old_sum = new_sum
    end
    max = [old_sum, new_sum].max
    highest = max if highest.nil? || highest < max
  end
  highest
end

def shuffle(arr)
  arr.each_index do |i|
    swap = rand(52)
    temp = arr[swap]
    arr[swap] = arr[i]
    arr[i] = temp
  end
  arr
end

def sum_two_largest(arr)
  bigger = nil
  biggest = nil
  arr.each do |a|
    if !biggest.nil? && a > biggest
      bigger = biggest
      biggest = a
    elsif !bigger.nil? && a > bigger
      bigger = a
    elsif biggest.nil?
      biggest = a
    elsif bigger.nil?
      bigger = a
    end
  end
  bigger + biggest
end

class Queue
  attr_accessor :first, :last

  def initialize(data)
    @first = @last = Node.new(data)
  end

  def push(d)
    node = Node.new(d)
    node.left = @last
    @last.right = node unless @last.nil?
    @last = node
    @first = node if @first.nil?
  end

  def pop
    return if @first.nil?
    popped = @first
    if @first.right.nil?
      # last one
      @first = nil
    else
      @first = @first.right
      @first.left = nil
    end
    @last = nil if @first.nil?
    popped
  end
end

class Node
  attr_accessor :left, :right, :data

  def initialize(d)
    @data = d
  end
end

# xor arrays
class Array
  def xor(arr)
    xored = []
    hash_arr = {}
    self.each do |el|
      hash_arr[el] = true
    end
    arr.each do |element|
      xored << element if hash_arr[element]
    end
    xored
  end
end

# breadth first search
class Node
  attr_accessor :left, :right, :val
  def initialize(r = nil, l = nil, v = nil)
    @right = r
    @left = l
    @val = v
  end

  def self.bfs(needle, haystack)
    return nil if haystack.nil?
    return haystack if needle == haystack.val
    found = nil
    found = bfs needle, haystack.left
    return found unless found.nil?
    found = bfs needle, haystack.right
    return found unless found.nil?
    nil
  end

  def self.dfs(needle, haystack)
    return nil if haystack.nil?
    found = nil
    found = dfs needle, haystack.left
    return found unless found.nil?
    found = dfs needle, haystack.right
    return found unless found.nil?
    return haystack if needle == haystack.val
    nil
  end
end

start = Node.new(
    Node.new(
        Node.new(nil, nil, 19),
        Node.new(nil, nil, 15),
        98
    ),
    Node.new(
        Node.new(nil, nil, 54),
        Node.new(nil, nil, 43),
        72
    ),
    4
)

# project euler 1
def multiples_of_x(num, x)
  len = num / x
  multiples = []
  len.times do |i|
    multiples << x * (i + 1)
  end
  multiples
end

[multiples_of_x(999, 3), multiples_of_x(999, 5)].flatten.uniq.inject(0){|i, sum| sum + i}

# project euler 2
def fib(num1 = 0, num2 = 1, max = 4_000_000, collector = [])
  collector << num2
  return collector if num1 + num2 > max
  fib(num2, num1 + num2, max, collector)
end

def add_even
  fib.select{|n| n % 2 == 0}.inject(0){|i, sum| sum + i}
end

# project euler 3
require 'prime'
def largest_prime_factors(num)
  factors = []
  stop = (num ** 0.5).ceil
  stop.times do |i|
    i += 1
    integer_divide = num / i
    factors << i if i * integer_divide == num
  end
  factors.select{|n| n.prime?}.max
end

# project euler 4
class Fixnum
  def palindrome?
    num = self
    puts "testing #{num}"
    i = 0
    while i < (num.to_s.length / 2)
      return false unless num.to_s[i] == num.to_s[num.to_s.length - 1 - i]
      i += 1
    end
    true
  end
end

def largest_three_digit_product_palindrome
  palindromes = []
  i = 999
  while i > 99
    j = 999
    while j > 99
      palindromes << (i * j) if (i * j).palindrome?
      j -= 1
    end
    i -= 1
  end
  palindromes.max
end

# project euler 5

def smallest_multiple(num)
  smallest = num
  while true
    i = num
    while i > 0
      divided = smallest / i.to_f
      unless divided == divided.to_i
        break
      end
      i -= 1
      return smallest if i == 0
    end
    smallest += num
  end
end

# project euler 6

def sum_square_diff(num)
  num = Array(1..num)
  sum_squares = num.inject(0) {|sum, i| sum + (i ** 2)}
  square_sums = num.inject(0) {|sum, i| sum + i} ** 2
  (sum_squares - square_sums).abs
end

# project euler 7

require 'prime'
def nth_prime(n)
  prime_nums = 0
  i = 0
  while prime_nums < n
    i += 1
    prime_nums += 1 if i.prime?
  end
  i
end

# project euler 8
large_number = <<-number
73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
number
large_number.gsub!(/\n/, '')

def largest_adjacent_product(string_digits, num_digits_to_multiply)
  max = 0
  len = string_digits.length
  i = 0
  while len > i
    stop = i + num_digits_to_multiply - 1 > len ? len - 1 : i + num_digits_to_multiply - 1
    product = string_digits[i..stop].chars.inject(1) {|sum, digit| sum * digit.to_i}
    if product != 0
      puts "nonzero: #{product}"
    end
    if product > max
      puts "i: #{i} -- string: #{string_digits[i..stop]} -- product: #{product} -- max: #{max}"
      max = product
    end
    i += 1
  end
  max
end

# project euler 9
def pythagorean_triplet(num)
  a = 0
  while a < num
    a += 1
    # puts "a: #{a}"
    b = a
    while b < num
      b += 1
      # puts "b: #{b}"
      next if b <= a
      c = b
      while c < num
        c += 1
        # puts "c: #{c}"
        next if c <= b || c <= a
        if a ** 2 + b ** 2 == c ** 2 && (a + b + c) == num
          puts "triplet found: #{a}, #{b}, #{c}"
          return a * b * c
        end
      end
    end
  end
end

# project euler 10
require 'prime'
def prime_summation(max)
  primes = []
  i = 1
  while i < max
    primes << i if i.prime?
    i += 2
  end
  primes.inject(0){|sum, prime| sum + prime} + 2
end

# project euler 11
grid = [
  [ 8,  2, 22, 97, 38, 15,  0, 40,  0, 75,  4,  5,  7, 78, 52, 12, 50, 77, 91,  8],
  [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48,  4, 56, 62,  0],
  [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30,  3, 49, 13, 36, 65],
  [52, 70, 95, 23,  4, 60, 11, 42, 69, 24, 68, 56,  1, 32, 56, 71, 37,  2, 36, 91],
  [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
  [24, 47, 32, 60, 99,  3, 45,  2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
  [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
  [67, 26, 20, 68,  2, 62, 12, 20, 95, 63, 94, 39, 63,  8, 40, 91, 66, 49, 94, 21],
  [24, 55, 58,  5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
  [21, 36, 23,  9, 75,  0, 76, 44, 20, 45, 35, 14,  0, 61, 33, 97, 34, 31, 33, 95],
  [78, 17, 53, 28, 22, 75, 31, 67, 15, 94,  3, 80,  4, 62, 16, 14,  9, 53, 56, 92],
  [16, 39,  5, 42, 96, 35, 31, 47, 55, 58, 88, 24,  0, 17, 54, 24, 36, 29, 85, 57],
  [86, 56,  0, 48, 35, 71, 89,  7,  5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
  [19, 80, 81, 68,  5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77,  4, 89, 55, 40],
  [ 4, 52,  8, 83, 97, 35, 99, 16,  7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
  [88, 36, 68, 87, 57, 62, 20, 72,  3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
  [ 4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18,  8, 46, 29, 32, 40, 62, 76, 36],
  [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74,  4, 36, 16],
  [20, 73, 35, 29, 78, 31, 90,  1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57,  5, 54],
  [ 1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52,  1, 89, 19, 67, 48]
]

def find_largest_adjacents(array, num, start_x = 0, start_y = 0, max_position = {value: 0})
  if start_x > array[0].length - 1
    start_x = 0
    start_y += 1

    if start_y > array.length - 1
      return max_position
    end
  end

  position_data = largest_direction array, num, start_x, start_y
  if !position_data.empty? && position_data[:value] > max_position[:value]
    max_position = position_data
  end
  find_largest_adjacents(array, num, start_x + 1, start_y, max_position)
end

def multiply(array, positions)
  product = 1
  positions.each do |x, y|
    product *= array[y][x]
  end
  product
end

def largest_direction(array, num, start_x, start_y)
  max = 0
  positions = []
  dirs = ['up', 'down', 'left', 'right', 'nw', 'ne', 'se', 'sw']
  dirs.each do |direction|
    product = check_direction(array, num, start_x, start_y, direction)
    if product > max
      max = product
      positions = {x: start_x, y: start_y, dir: direction, value: max}
    end
  end
  positions
end

def check_direction(array, num, start_x, start_y, direction)
  largest_x = array[0].length - 1
  largest_y = array.length - 1
  positions = []

  case direction
    when 'up'
      num.times{|i| positions << [start_x, start_y - i]}
    when 'down'
      num.times{|i| positions << [start_x, start_y + i]}
    when 'left'
      num.times{|i| positions << [start_x - i, start_y]}
    when 'right'
      num.times{|i| positions << [start_x + i, start_y]}
    when 'nw'
      num.times{|i| positions << [start_x - i, start_y - i]}
    when 'ne'
      num.times{|i| positions << [start_x + i, start_y - i]}
    when 'se'
      num.times{|i| positions << [start_x + i, start_y + i]}
    when 'sw'
      num.times{|i| positions << [start_x - i, start_y + i]}
  end

  if !positions.map{|i, j| i}.flatten.detect{|i| i < 0 || i > largest_x} &&
     !positions.map{|i, j| j}.flatten.detect{|j| j < 0 || j > largest_y}
    multiply(array, positions)
  else
    0
  end
end

# project euler 12
def factors(num)
  list = []
  ((num + 1) / 2.0).ceil.times do |i|
    next if i == 0
    quotient = num / i.to_f
    if quotient == quotient.floor
      list << i
      list << quotient.floor
    end
    return list if i >= quotient.floor
  end
  list.uniq
end

def triangle_number(num)
  sum = 0
  (num + 1).times do |i|
    sum += i
  end
  sum
end

def over_500
  triangle = 1
  tri_i = 2
  while factors(triangle).length <= 500
    triangle += tri_i
    tri_i += 1
  end
  triangle
end

# project euler 13
huge_nums = %w(
37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690
)

def huge_sum_first_ten_digits(num_list)
  nums = num_list.map{|num| num.to_i}
  nums.inject(0) {|sum, i| sum + i}.to_s[0..9]
end

# project euler 14
def collatz_sequence(num, sequence = [])
  sequence << num
  return sequence if num == 1
  if num.odd?
    collatz_sequence(num * 3 + 1, sequence)
  else
    collatz_sequence(num / 2, sequence)
  end
end

def longest_collatz(max_num)
  longest = []
  (max_num + 1).times do |i|
    seq = collatz_sequence(i + 1)
    longest = seq if seq.length > longest.length
  end
  longest
end

# project euler 15
def lattice_paths(size_x, size_y)
  grid = Array.new(size_x + 1) { Array.new(size_y + 1) {0} }
  grid[size_x][size_y] = 1

  cur_x = size_x

  while cur_x >= 0
    cur_y = size_y
    while cur_y >= 0
      val = add_adjacent(grid, cur_x, cur_y)
      puts "#{cur_x}, #{cur_y} = #{val}"
      grid[cur_x][cur_y] = val unless cur_x == size_x && cur_y == size_y # bottom right is always 1, ignore it
      cur_y -= 1
    end
    cur_x -= 1
  end
  grid[0][0]
end

def add_adjacent(grid, x, y)
  x_length = grid.length
  y_length = grid[0].length
  adj_x = if x + 1 >= x_length
    0
  else
    grid[x + 1][y]
  end
  adj_y = if y + 1 >= y_length
    0
  else
    grid[x][y + 1]
  end
  adj_x + adj_y
end

# project euler 16
def power_digit_sum(num, power)
  big_num = num ** power
  big_num.to_s.chars.inject(0) {|sum, i| sum + i.to_i}
end

# project euler 17
require 'byebug'
class Fixnum
  def to_words
    places = {3 => 'hundred', 4 => 'thousand'}
    ones = {'0' => '', '1' => 'one', '2' => 'two', '3' => 'three', '4' => 'four', '5' => 'five', '6' => 'six', '7' => 'seven', '8' => 'eight', '9' => 'nine'}
    teens = {'10' => 'ten', '11' => 'eleven', '12' => 'twelve', '13' => 'thirteen', '14' => 'fourteen',
             '15' => 'fifteen', '16' => 'sixteen', '17' => 'seventeen', '18' => 'eighteen', '19' => 'nineteen'}
    tens = {'2' => 'twenty', '3' => 'thirty', '4' => 'forty', '5' => 'fifty', '6' => 'sixty', '7' => 'seventy', '8' => 'eighty', '9' => 'ninety'}

    return 'zero' if self == 0
    words = ''
    digits = self.to_s.chars

    while digits.length > 2
      if digits.length >= 3
        d = digits[0]
        if d != '0'
          words += ones[d] + ' '
          words += places[digits.length] + ' '
          words += 'and ' if digits.length == 3 && (digits[1] != '0' || digits[2] != '0')
        end
        digits = digits[1..-1]
      end
    end

    if digits.length == 2
      if digits[0] == '1'
        words += teens[digits[0] + digits[1]] + ' '
        return words.strip
      # elsif digits[0] == '0' && digits[1] == '0'
      #   return words.strip
      elsif digits[0] == '0'
      #   words += ones[digits[1]] + ' '
      #   return words.strip
      else
        words += tens[digits[0]] + ' '
      end
      digits = digits[1..-1]
    end

    words += ones[digits[0]]
    words.strip
  end
end

require 'test/unit'

class TestNumToWords < Test::Unit::TestCase
  def test_to_words_0
    assert_equal 'zero', 0.to_words
  end

  def test_to_words_1
    assert_equal 'one', 1.to_words
  end

  def test_to_words_27
    assert_equal 'twenty seven', 27.to_words
  end

  def test_to_words_100
    assert_equal 'one hundred', 100.to_words
  end

  def test_to_words_112
    assert_equal 'one hundred and twelve', 112.to_words
  end

  def test_to_words_890
    assert_equal 'eight hundred and ninety', 890.to_words
  end

  def test_to_words_894
    assert_equal 'eight hundred and ninety four', 894.to_words
  end

  def test_to_words_1000
    assert_equal 'one thousand', 1000.to_words
  end
end

def sum_word_numbers(max)
  length_sum = 0
  max.times do |i|
    puts "#{(i + 1).to_words.gsub(/ /, '')} = #{(i + 1).to_words.gsub(/ /, '').length}"
    length_sum += (i + 1).to_words.gsub(/ /, '').length
  end
  length_sum
end

# project euler 18 & 67 (just change the file being read in)
def read_triangle_file
  triangle_array = []
  file = File.open('euler_67.txt', 'r')
  file.each_line do |line|
    triangle_array << line.split(' ')
  end
  triangle_array
end

def bottom_to_top_maximums
  triangle_array = read_triangle_file
  cur_row = triangle_array.length - 2
  row_array = triangle_array.last

  while cur_row >= 0
    new_array = []
    triangle_array[cur_row].each_index do |i|
      new_array << [row_array[i].to_i, row_array[i + 1].to_i].max + triangle_array[cur_row][i].to_i
    end
    row_array = new_array
    cur_row -= 1
  end
  row_array
end

# project euler 19
def twentieth_century_sundays
  # 1900/01/01 was a Monday, 1900 is not a leap year
  # thus 364 days later (1900/12/31) was a Monday, and 1901/01/06 was a Sunday
  year = 1901
  month = 0
  day = 6
  first_of_month_sunday = 0
  while year < 2001
    days_of_months = [31, leap_year?(year) ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    while day <= days_of_months[month]
      day += 7
      if day > days_of_months[month]
        day -= days_of_months[month]
        month += 1
        first_of_month_sunday += 1 if day == 1
        puts "Sunday on #{year}/#{month + 1}/#{day}" if day == 1
      end
      break if month > days_of_months.length - 1
    end
    year += 1
    month = 0
  end
  first_of_month_sunday
end

def leap_year?(year)
  year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
end

# project euler 20
def factorial_digits_sum(n)
  product = 1

  n.times do |n|
    product *= (n + 1)
  end

  product.to_s.chars.inject(0) {|sum, i| sum + i.to_i }
end

# project euler 21
def proper_divisors(num)
  list = []
  (num - 1).times do |n|
    list << (n + 1) if num % (n + 1) == 0
  end
  list
end

def find_amicable_numbers(max)
  amicable_nums = []
  max.times do |num|
    sum = proper_divisors(num + 1).inject(0) {|sum, i| sum + i}
    amicable_nums << num + 1 if proper_divisors(sum).inject(0) {|sum, i| sum + i} == (num + 1) && sum != (num + 1)
  end
  amicable_nums.inject(0) {|sum, i| sum + i}
end

# project euler 22
def read_names(filename)
  File.open(filename, 'r').read.gsub(/"/, '').split(',').sort
end

def name_to_int(name)
  score = 0
  name.upcase.each_byte do |num|
    score += num - 'A'.bytes[0] + 1
  end
  score
end

def calculate_score
  scores = []
  names = read_names 'euler_22.txt'
  names.each_index do |i|
    scores << name_to_int(names[i]) * (i + 1)
  end
  scores.inject(0) {|sum, i| sum + i}
end

# project euler 23
def proper_divisors(num)
  list = []
  (num - 1).times do |n|
    list << (n + 1) if num % (n + 1) == 0
  end
  list
end

MAX_NON_ABUNDANT = 28_123
def non_abundant_sums
  abundants = abundant_nums MAX_NON_ABUNDANT
  non_abundant = []
  MAX_NON_ABUNDANT.times do |n|
    puts n + 1
    abundant_match_found = false
    abundants.each do |abundant|
      break if abundant > n + 1
      possible_match = n + 1 - abundant
      if abundants.include?(possible_match)
        abundant_match_found = true
        break
      end
    end
    non_abundant << n + 1 unless abundant_match_found
  end
  non_abundant
end

def abundant_nums(max)
  list = []
  max.times do |n|
    sum = proper_divisors(n + 1).inject(0) {|sum, i| sum + i}
    list << n + 1 if sum > n + 1
  end
  list
end

# project euler 24
def lexicographic_permutations
  [0,1,2,3,4,5,6,7,8,9].permutation.sort[999_999].join
end

# project euler 25
def fibonacci_digits(digits)
  two_back = 1
  one_back = 1
  i = 2
  while one_back.to_s.length < digits
    num = one_back + two_back
    two_back = one_back
    one_back = num
    i += 1
  end
  i
end

# project euler 26
require 'bigdecimal'
def longest_recurring_decimal(max)
  longest_recurring = ''
  longest_num = nil
  max.times do |i|
    decimal = (BigDecimal.new(10 ** 10_000) / BigDecimal.new(i + 1)).to_s('F').gsub(/\./, '')
    loop = looping_string decimal
    if loop.length > longest_recurring.length
      longest_recurring = loop
      longest_num = i + 1
    end
  end
  longest_num
end

def looping_string(string)
  start = 0
  length = 1
  looped_str = ''
  matching = false

  while test = string.slice(start, length)
    break if (matching && test == looped_str) || start >= string.length
    index = string[start + 1..-1].index(test)
    if !index.nil?
      index += 1
      looped_str = string.slice(start, index)
      start = index + start
      length = looped_str.length
      matching = true
    else
      matching = false
      looped_str = ''
      start += 1
    end
  end

  looped_str
end

# project euler 27
def quadratic_checker(max)
  best = {a: 0, b: 0, n: 0, primes: 0}

  a = -1000
  while a < max
    b = -1000
    while b < max
      cur_primes = 0
      n = 0
      while n < max
        if (n ** 2 + a * n + b).prime?
          cur_primes += 1
          n += 1
        else
          break
        end
      end
      if cur_primes > best[:primes]
        best[:a] = a
        best[:b] = b
        best[:n] = n
        best[:primes] = cur_primes
      end
      b += 1
    end
    a += 1
  end
  best[:a] * best[:b]
end

# project euler 28
def number_spiral(size)
  last_num = size ** 2
  i = 1
  increment = 2
  sum = i
  four_corners = 4
  while i < last_num
    i += increment
    sum += i
    four_corners -= 1
    if four_corners < 1
      increment += 2
      four_corners = 4
    end
  end
  sum
end

# project euler 29
def distinct_powers(max)
  powers = []
  a = 2
  while a <= max
    b = 2
    while b <= max
      powers << a ** b
      b += 1
    end
    a += 1
  end
  powers.uniq.length
end

# project euler 30
def digit_fifth_powers(max)
  powers = []
  i = 10
  while i <= max
    powers << i if i == fifth_power(i)
    i += 1
  end
  powers.inject(0) {|sum, i| sum + i}
end

def fifth_power(num)
  sum = 0
  num.to_s.each_char do |char|
    sum += char.to_i ** 5
  end
  sum
end

# project euler 31
def coin_sums(amount)
  order = [200, 100, 50, 20, 10, 5, 2, 1]
  zeroed_coins = {1 => 0, 2 => 0, 5 => 0, 10 => 0, 20 => 0, 50 => 0, 100 => 0, 200 => 0}
  coins = zeroed_coins.dup
  good_combos = []
  current = 0
  coins[200] = amount / 200

  while true
    if sum_collection(coins) == amount
      good_combos << coins.dup
      break if coins.reject{|key, val| val == 0}.keys == [1] && current == order.length - 1 && sum_collection(coins) == amount
      if no_larger_nonzeros?(coins, current)
        current += 1
        temp = zeroed_coins.dup
        temp[order[current - 1]] = coins[order[current - 1]] - 1 if coins[order[current - 1]] > 0
        coins = temp
        coins[order[current]] = (amount - sum_collection(coins)) / order[current]
        next
      else
        if order[current] == 1
          coins[order[current]] = 0
          while coins[order[current]] == 0 && current > 0 && current < order.length
            current -= 1
          end
          coins[order[current]] -= 1 if coins[order[current]] > 0
        else
          coins[order[current]] -= 1 if order[current] != 1 && coins[order[current]] > 0
        end
      end
      current += 1
      sum = sum_collection(coins)
      coins[order[current]] = (amount - sum) / order[current]
      next
    end
    if sum_collection(coins) < amount
      if sum_collection(coins) + order[current] > amount
        # no more of the current coin, try a smaller one
        current += 1
        coins[order[current]] += 1
      else
        # keep incrementing current coin
        coins[order[current]] += 1
      end
    end
  end
  good_combos.uniq
end

def no_larger_nonzeros?(coins, current)
  order = [200, 100, 50, 20, 10, 5, 2, 1]
  i = 0
  while i < current
    return false if coins[order[i]] != 0
    i += 1
  end
  true
end

def sum_collection(collection)
  collection[1] + 2 * collection[2] + 5 * collection[5] + 10 * collection[10] + 20 * collection[20] +
    50 * collection[50] + 100 * collection[100] + 200 * collection[200]
end

# project euler 32
def pandigital_products
  products = []
  100.times do |multiplicand|
    10_000.times do |multiplier|
      product = multiplicand * multiplier
      products << product if is_pandigital? [multiplicand, multiplier, product]
    end
  end
  products.uniq.inject(0) {|sum, i| sum + i}
end

def is_pandigital?(nums)
  chars = nums.flatten.inject(''){|str, num| str + num.to_s}
  return false unless chars.index('0').nil?

  digits = %w|1 2 3 4 5 6 7 8 9|
  chars.each_char do |digit|
    return false if digits.index(digit).nil?
    digits.delete digit
  end
  digits.empty?
end

# project euler 33
def digit_canceling_fractions
  list = []
  numerator = 10
  while numerator < 100
    denominator = 10
    while denominator < 100
      if numerator < denominator
        canceled = cancelable_digits?(numerator, denominator)
        if canceled && (canceled[0].to_i / canceled[1].to_f) == (numerator.to_i / denominator.to_f)
          list << [numerator, denominator]
        end
      end
      denominator += 1
    end
    numerator += 1
  end
  list
end

def cancelable_digits?(a, b)
  a = a.to_s
  b = b.to_s
  return false if a == b || a.length != 2 || b.length != 2 || (a[1] == '0' && b[1] == '0' && a[0] != b[0])
  if b.index(a[0]) == 0
    [a[1], b[1]]
  elsif b.index(a[0]) == 1
    [a[1], b[0]]
  elsif b.index(a[1]) == 0
    [a[0], b[1]]
  elsif b.index(a[1]) == 1
    [a[0], b[0]]
  end
end

# project euler 34
def digit_factorials(max)
  list = []
  i = 3
  while i < max
    puts "." if i % 10_000 == 0
    product = 0
    i.to_s.each_char do |n|
      product += factorial n
    end
    list << i if product == i
    i += 1
  end
  list
end

def factorial(n)
  product = 1
  n.to_i.times do |n|
    product *= n + 1
  end
  product
end

# project euler 35
require 'prime'
def circular_primes(max)
  list = []
  max.times do |n|
    initial = n.to_s
    all_primes = true
    last_time = false
    i = 1
    while !last_time
      last_time = true if initial == (rotated = initial.split('').rotate(i)).join('')
      if !rotated.join('').to_i.prime?
        all_primes = false
        break
      end
      i += 1
    end
    list << n if all_primes
  end
  list
end

# project euler 36
def double_base_palindromes(max)
  list = []
  max.times do |num|
    list << num if palindrome?(num.to_s) && palindrome?(num.to_s(2))
  end
  list.inject(0) {|sum, i| sum + i}
end

def palindrome?(str)
  while str.length > 1
    return false if str[0] != str[str.length - 1]
    str = str[1..-2]
  end
  true
end

# project euler 37
def truncatable_primes(max)
  list = []
  num = 11
  while num < max
    num += 2
    if (index = num.even_digit_pos) && num.to_s.length > 2
      if index + 1 != num.to_s.length - 1
        num_str = num.to_s
        num_str[index] = (num_str[index].to_i + 1).to_s
        num = num_str.to_i
      end
      num.to_s.length - index
      next
    end
    list << num if num.front_and_back_prime?
    break if list.length == 11
  end
  puts list
  list.inject(0) {|sum, i| sum + i}
end

class Fixnum
  def even_digit_pos
    num_str = self.to_s
    i = 0
    while i < num_str.length
      char = num_str[i]
      if i == 0 && char == '2'
        i += 1
        next
      end
      return i if '02468'.index(char) != nil
      i += 1
    end
    nil
  end

  def front_and_back_prime?
    return false unless self.prime?
    num_str = self.to_s
    num_str.length.times do |i|
      i += 1
      return false unless num_str.slice(0, i).to_i.prime?
      return false unless num_str.slice(-1 * i, num_str.length).to_i.prime?
    end
    true
  end
end

# project euler 38
def pandigital_multiples
  largest = 0
  1_000_000.times do |num|
    num += 1
    result = ''
    sequence = 1
    while result.to_i < 1_000_000_000
      result += (num * sequence).to_s
      if result.pandigital?
        largest = result.to_i if result.to_i > largest
      end
      sequence += 1
    end
  end
  largest
end

class String
  def pandigital?
    return false if self.length != 9
    digits = %w|1 2 3 4 5 6 7 8 9|
    self.each_char do |char|
      return false if digits.delete(char) == nil
    end
    digits.empty?
  end
end

# project euler 39
def integer_right_triangles
  best = 0
  best_perimeter = 0

  1000.times do |perimeter|
    solutions = []
    perimeter += 1
    (perimeter/2).ceil.times do |a|
      a += 1
      (perimeter/2).ceil.times do |b|
        b += 1 + a
        next if a > b
        (perimeter/2).ceil.times do |c|
          c += 1 + b
          next if a > c || b > c || a + b + c != perimeter
          solutions << [a, b, c] if a ** 2 + b ** 2 == c ** 2
        end
      end
    end
    if solutions.length > best
      best = solutions.length
      best_perimeter = perimeter
    end
  end
  best_perimeter
end

# project euler 40
def champernowne
  next_digit = 1
  product = 1
  digit_count = 1
  1_000_000.times do |i|
    i += 1
    if digit_count + i.to_s.length > next_digit
      product *= i.to_s[next_digit - digit_count].to_i
      next_digit *= 10
    end
    digit_count += i.to_s.length
  end
  product
end

# project euler 41
require 'prime'
def pandigital_primes
  largest = 0
  digits = %w|1 2 3 4 5 6 7 8 9|
  while digits.length > 0
    digits.permutation.each do |sample|
      sample_num = sample.join.to_i
      largest = sample_num if sample_num > largest && sample_num.prime?
    end
    digits.pop
  end
  largest
end

# project euler 42
def triangle_words
  triangle_numbers = []
  30.times do |n|
    n += 1
    triangle_numbers << (0.5 * n * (n + 1)).to_i
  end
  words = File.open('euler_42.txt', 'r').read.gsub(/"/, '').split(',')
  words.map(&:sum).reject{|num| ([num] & triangle_numbers).empty?}.length
end

class String
  def sum
    self.upcase.chars.inject(0) {|sum, char| sum + (char.ord - 'A'.ord + 1)}
  end
end

# project euler 43
def substring_divisibility
  %w|0 1 2 3 4 5 6 7 8 9|.permutation.map(&:join).map(&:to_i).select(&:prime_divisible?).inject(0) {|sum, i| sum + i}
end

class Integer
  def prime_divisible?
    divisible_sets = {1 => 2, 2 => 3, 3 => 5, 4 => 7, 5 => 11, 6 => 13, 7 => 17}
    divisible_sets.each do |key, value|
      return false if self.to_s.slice(key, 3).to_i % value != 0
    end
    true
  end
end