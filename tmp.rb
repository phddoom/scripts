#! /usr/bin/env ruby

require 'shellwords'

files = Dir.glob "*/*.[A-z][A-z][A-z]"
%x{mplayer #{Shellwords.join files[rand files.size]}}
