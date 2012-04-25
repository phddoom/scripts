#! /usr/bin/env ruby
Encoding.default_external = Encoding::UTF_8
Encoding.default_internal = Encoding::UTF_8
require 'shellwords'

files = Dir.glob "*/*.[A-z][A-z][A-z]"
num_of_plays = ARGV.first
array_of_rands = Array.new num_of_plays.to_i if num_of_plays
array_of_rands ||= [nil]
STDOUT.puts array_of_rands.inspect
array_of_rands.map!{|a| a = rand files.size}
STDOUT.puts array_of_rands.inspect
array_of_rands.uniq!
while array_of_rands.size < num_of_plays.to_i
  array_of_rands << (rand files.size)
  array_of_rands.uniq!
end
STDOUT.puts array_of_rands.inspect
arg = (files.values_at *array_of_rands).map{|a| Shellwords.shellescape a}.join(" ")
%x{mplayer -fs #{arg}}
