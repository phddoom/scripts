#! /usr/bin/env ruby

#ding!
#A utility to alert the user that a command/process has completed

require 'getoptlong'
ENV["POSIXLY_CORRECT"]="true"

def help
  puts "Long\tShort\tDescription"
  puts "--help\t-h\tThis help"
  puts "--dzen\t-d\tPass options as a string to dzen"
end

opts = GetoptLong.new(
  ["--help", "-h", GetoptLong::NO_ARGUMENT],
  ["--dzen", "-d", GetoptLong::REQUIRED_ARGUMENT]
)

displayer_args = "-p 10 -w 300 -l 4 -sa c -e 'onstart=uncollapse;button1=exit'"
displayer = "dzen2"
command = String.new


opts.each do |opt, arg|
  case opt
  when "--help"
    help
    exit
  when "--dzen"
    displayer = "dzen2"
    displayer_args = arg
  end
end


output = `#{ARGV.join " "}`

status = $?.exitstatus

`echo -e -n #{output.dump} | #{displayer} #{displayer_args}`

