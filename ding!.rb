#!/usr/bin/env ruby

#ding!
#A utility to alert the user that a command/process has completed

load '/home/odin/scripts/notify-dzen.rb'

handler = NotificationHandler.new

command = ARGV.join " "

output = `#{command}`
puts output

ding = Notification.new "Ding: #{ARGV.first} done", output
handler << ding
handler.notify

