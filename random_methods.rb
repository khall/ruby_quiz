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
  primes.inject(0){|sum, prime| sum + prime}
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