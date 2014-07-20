class Parser
  # attr_accessor :list

  def parse_list(input)
    raise "input is not of type IO" unless input.class == IO
    list = []
    input.readlines.each do |line|
      matched = line.match(/(\S+) +(\S+) +<(.+)>/)
      list << Person.new(matched[1], matched[2], matched[3])
    end
    list
  end
end

class Person
  attr_accessor :first_name, :last_name, :email, :chosen

  def initialize(first_name, last_name, email)
    @first_name = first_name
    @last_name = last_name
    @email = email
    @chosen = false
  end

  def pick(list)
    while picked = list[rand(list.length)]
      if picked.last_name != self.last_name && !picked.chosen
        picked.chosen = true
        return picked
      end
      return nil if list.select{|p| !p.chosen && p.last_name != self.last_name}.empty?
    end
  end

  def full_name
    "#{self.first_name} #{self.last_name}"
  end
end

require 'net/smtp'
class Emailer
  # never thought I'd write a bulk mailer...
  def self.bulk_mailer(list)
    smtp = Net::SMTP.new('smtp.gmail.com', 587)
    smtp.enable_starttls
    smtp.start('gmail.com', 'your_gmail@gmail.com', 'your_password', :login)
    list.each do |santa, secret|
      mail(santa, secret, smtp)
    end
    smtp.finish
  end

  def self.mail(santa, secret, smtp)
    message = [
      "From: Santa Picker <santa_picker@northpole.com>",
      "To: #{santa.full_name} <#{santa.email}>",
      "Subject: Secret Santa",
      "",
      "Dear Santa #{santa.full_name}, you shall present copious gifts to #{secret.full_name}"
    ].join("\r\n")

    # if testing, change santa.email to your email address to prevent spamming test addresses
    smtp.send_message message, 'secret_santa@northpole.com', santa.email
  end
end

class Santa
  def self.ho_ho_ho
    parser = Parser.new
    list = parser.parse_list $stdin
    pick_list = {}
    while !apply_rules(pick_list)
      pick_list = {}
      list.each do |p|
        pick_list[p] = p.pick list
      end
    end
    pick_list.each{|pr, pe| puts "#{pr.full_name} => #{pe.full_name}"}
    Emailer.bulk_mailer pick_list
  end

  # reject lists where a person has chosen another with the same last name
  def self.apply_rules list
    return if list.empty?
    list.each do |picker, pickee|
      if pickee.nil? || picker.last_name == pickee.last_name
        reset_chosen list
        return false
      end
    end
    true
  end

  def self.reset_chosen(list)
    list.each do |p, p2|
      next if p.nil?
      p.chosen = false
    end
  end
end

Santa.ho_ho_ho