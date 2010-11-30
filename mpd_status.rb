#! /usr/bin/env ruby

Dir.chdir("/home/odin/scripts")

require 'notify-dzen'
require 'dzen'
require 'librmpd'
require 'rubygems'
require 'i3-ipc'

UNFOCUS_BORDER = "#4c7899"
UNFOCUS_BACKGROUND = "#285577"
UNFOCUS_TEXT = "#ffffff"

FOCUS_BORDER = "#333333"
FOCUS_BACKGROUND = "#222222"
FOCUS_TEXT = "#888888"

def get_mpd_status mpd
  mpd.connect unless mpd.connected?
  song = mpd.current_song
  rep = mpd.repeat?.to_s.capitalize
  ran = mpd.random?.to_s.capitalize
  title_from_file = song.file.split("/").last.split(".").first
  status = mpd.playing? ? " MPD: #{song.title || title_from_file} by #{song.artist || "Unknown"} - REP: #{rep} - RAN: #{ran}" : " MPD: NOT PLAYING"
  return status
end

def file_to_hash filename
  hash = {}
  File.open(filename, 'r').each do |data|
    data.chop!
    key_value = data.split(":")
    key = key_value.first.gsub(" ", "_").to_sym
    value = key_value.last
    hash[key] = value.strip
  end
  return hash
end

def bat_display
 @PATH =  "/proc/acpi/battery/BAT0/"
 return " " unless File.exists? @PATH 
 status = " BAT: STATE: "
 state_hash = file_to_hash @PATH + "state"
 info_hash = file_to_hash @PATH + "info"
 status << state_hash[:charging_state]
 status << " PERCENT:"
 total = info_hash[:last_full_capacity].split(" ").first
 current = state_hash[:remaining_capacity].split(" ").first
 percent = ((current.to_f/total.to_f) * 100).round
 fg = case 
      when percent < 5
        "red"
      when percent < 25
        "yellow"
      else 
        "green"
      end
 status << (percent.to_s + "% ").fg(fg)
end

def chromo secs
  increment = 4.25
  iArrColors = Array.new(60);
  (0 .. 60).each do |index|
    iArrColors[index] = (index * increment).round
  end

  red, green, blue = case secs
                     when 0 ... 239
                       col = iArrColors[(60 - (secs/4))] || 0
                       [0, 255, col]
                     when 240
                       [0, 255, 0]
                     when 241 ..479
                       col = iArrColors[((secs - 240)/4)] || 0
                       [col, 255, 0]
                     when 480
                       [255, 255, 0]
                     when 481 .. 719
                       col = iArrColors[(60 - ((secs - 480)/4))] || 0
                       [255, col, 0]
                     when 720
                       [255, 0, 0]
                     when 721 ..959
                       col = iArrColors[(secs - 720)/4] || 0
                       [255, 0, col]
                     when 960
                       [255, 0, 255]
                     when 961 .. 1199
                       col = iArrColors[(60 - ((secs - 960)/4))] || 0
                       [col, 0, 255]
                     when 1200
                       [0, 0, 255]
                     when 1201 .. 1439
                       col = iArrColors[((secs - 1200)/4)] || 0
                       [0, col, 255]
                     else
                       [255, 255, 255]
                     end

  red = red.to_s(16) 
  green = green.to_s(16)
  blue = blue.to_s(16)

  red = "0" + red if red.length == 1
  green = "0" + green if green.length == 1
  blue = "0" + blue if blue.length == 1
  return "#" + red + green + blue
end

def time_status
  now = Time.now
  now.strftime("DATE:" + "%A %B %d %I:%M:%S %p %Y".fg(chromo((now.hour * 60) + now.min)))
end

def get_volume channel
  output = `amixer -c 0 get #{channel}`
  output = output.split("\n")[4].split(" ")
  if output.last.include?("[on]")
    return "^ca(1,amixer -c 0 set #{channel} toggle > /dev/null)^fg(green)#{output[3]} ^fg ^ca()"
  else
    return "^ca(1,amixer -c 0 set #{channel} toggle > /dev/null)^fg(red)#{output[3]} ^fg ^ca()"
  end
end

def volume_display
  status = "Sound: Master: " + get_volume("Master")
  #status << " Speaker: " << get_volume("Speaker")
  return status
end

def notify_current_song song
  handler = NotificationHandler.new
  song_body = String.new
  song.each do |k,v|
    if k != "title"
      song_body << (k + ": " + v + "\n") 
    end
  end
  # This convoluted assingment uses splits to get the file name
  # from the file path of the song
  title_from_file = song.file.split("/").last.split(".").first
  handler << Notification.new(song.title || title_from_file, song_body)
  handler.notify
end

def workspace i3
  ws_str = ""
  i3.get_workspaces.each do |ws|
    ws_str << case ws["focused"]
    #DRY this up
    #Also add urgency
    when true
      "^fg(#{UNFOCUS_BACKGROUND})^r(20x20)^fg()^p(-20)^ib(1)^fg(#{UNFOCUS_BORDER})^ro(21x21)^fg()^p(-14)^ib(1)^fg(#{UNFOCUS_TEXT})#{ws["name"]}^fg()".ca(1,"i3-msg '#{ws["name"]}' > /dev/null").strip + " "
    when false
      "^fg(#{FOCUS_BACKGROUND})^r(20x20)^fg()^p(-20)^ib(1)^fg(#{FOCUS_BORDER})^ro(21x21)^fg()^p(-14)^ib(1)^fg(#{FOCUS_TEXT})#{ws["name"]}^fg()".ca(1, "i3-msg '#{ws["name"]}' > /dev/null").strip + " "
    end
  end
  ws_str
end

def display
  #dzen options
  dzen = "dzen2 -p -dock "
  display_settings = "-xs 1 -ta l"
  actions = " -e 'onstart=lower;button4=scrollup;button5=scrolldown;'"
  font = " -fn 'Terminus-10'"
  

  #mpd setup
  @mpd = MPD.new
  @mpd.connect true
  @mpd.register_callback(Object.method('notify_current_song'), MPD::CURRENT_SONG_CALLBACK)
  
  #I3 setup
  @i3 = I3::IPC.new

  #I herd you like pipes ...
  IO.popen dzen + display_settings + actions + font, "w" do |pipe|
    while true
      display = workspace(@i3) + time_status + get_mpd_status(@mpd) + bat_display + volume_display
      pipe.puts display
      #sleep 0.25 
    end
  end

end

display
