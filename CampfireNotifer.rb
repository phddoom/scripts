#!/usr/bin/env ruby

require 'rubygems'
require 'tinder'

class String
  def escape
    return self.dump[1..-2]
  end
end

campfire = Tinder::Campfire.new 'mobiwireless', :token => "e41e1ac8bed6085751c4e5c1ea1e346bcf6ebe47", :ssl => true

room = campfire.rooms.first

room.listen do |message|
  title = "Campfire Message"
  user = message[:user]
  if message[:body]
    body = user[:name] + ": " + message[:body].escape
  else
    body = "Something has been posted or pasted."
  end
  options = "-t #{1 * 1000}" #-i #{stock_icon}
  system "notify-send #{options} '#{title}' '#{body}'"
end
