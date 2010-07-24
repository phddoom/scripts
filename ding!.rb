#! /usr/bin/env ruby

#ding!
#A utility to alert the user that a command/process has completed

require 'getoptlong'

opts = GetoptLong.new(
  ["--help", "-h", GetoptLong::NO_ARGUMENT],
  ["--dzen", "-d", GetoptLong::REQUIRED_ARGUMENT]
)

displayer_args = "-p 10 -w 400 -l 18 -sa c -e 'onstart=uncollapse;button1=exit'"
displayer = "dzen2"
command = String.new

opts.each do |opt, arg|
  case opt
  when "--help"
    exit
  when "--dzen"
    displayer = "dzen2"
    displayer_args = arg
  end
end


output = `#{ARGV.first}`

status = $?.exitstatus

`echo #{output.dump} | #{displayer} #{displayer_args}`

def help
end
