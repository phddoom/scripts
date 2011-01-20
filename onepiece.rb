#! /usr/bin/env ruby

require 'rubygems'
require 'mechanize'

agent = Mechanize.new

episode_page = agent.get "http://www.animecrazy.net/one-piece-episode-#{ARGV[0]}"
links = episode_page.links_with(:text => " HD Download with English Subs (Megaupload)")
download_link = links[links.size - 2]
download_link.attributes.each do |k,v|
  puts k + ": " + v
end
puts download_link.text
download_link.attributes[:onclick] =~ /download\/(\d*)/
true_download_link = "http://www.animecrazy.net/mirrordownload/" + $1
puts true_download_link
download_page = agent.get true_download_link
megaupload_link = download_page.link_with(:href => /files/)
puts megaupload_link.href
puts "Getting Response"
episode = agent.get megaupload_link.href do |page|
puts "Save File to Disk"
episode.save_as "One Piece/"+ megaupload_link.href.split("/").last
puts "Exiting"
