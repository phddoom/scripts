#!/usr/bin/env ruby

require 'rubygems'
require 'tinder'
require 'notify-dzen'

class String
  def escape
    return self.dump[1..-2]
  end
end

handler = NotificationHandler.new

campfire = Tinder::Campfire.new 'mobiwireless', :token => "e41e1ac8bed6085751c4e5c1ea1e346bcf6ebe47", :ssl => true

room = campfire.rooms.first

room.listen do |message|
  title = "Campfire Message"
  user = message[:user]
  body = user[:name] + ": " + message[:body].escape if message[:body]
  body ||= "Something posted"
  handler << Notification.new(title, body)
  handler.notify
end
