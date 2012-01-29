#! /usr/bin/env ruby

require 'rubygems'
require_relative 'notify-dzen'
require_relative 'dzen'
require 'librmpd'


def get_mpd_status mpd
  begin
    mpd.connect unless mpd.connected?
  rescue
    return " MPD: NOT CONNECTED"
  end
  song = mpd.current_song
  rep = mpd.repeat?.to_s.capitalize
  ran = mpd.random?.to_s.capitalize
  title_from_file = song.file.split("/").last.split(".").first if song
  status = mpd.playing? ? " MPD: #{song.title || title_from_file} by #{song.artist || "Unknown"} - REP: #{rep} - RAN: #{ran}" : " MPD: NOT PLAYING"
  return status
end

def file_to_hash filename
  hash = {}
  File.open(filename, 'r').each do |data|
    data.chop!
    key_value = data.split(":")
    key = key_value.first.gsub(" ", "_").downcase.to_sym
    value = key_value.last
    hash[key] = value.strip
  end
  hash
end

def bat_display
 @PATH =  "/sys/class/power_supply/BAT0/"
 return "" unless File.exists? @PATH
 status = " BAT: "
 charge_now = File.open(@PATH + "/charge_now", "r").gets.strip.to_f
 charge_full = File.open(@PATH + "/charge_full", "r").gets.strip.to_f
 percent = (charge_now / charge_full) * 100
 fg = case
      when percent < 5
        "red"
      when percent < 25
        "yellow"
      else
        "green"
      end
 percent_string = "%0.2f%" % percent
 status << percent_string.fg(fg)
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
    return "#{output[3]}".fg("green").ca("1", "amixer -c 0 set #{channel} toggle")
  else
    return "#{output[3]}".fg("red").ca("1", "amixer -c 0 set #{channel} toggle")
  end
end

def volume_display
  status = " Sound: Master: " + get_volume("Master")
  return status
end

@cpu ={}
def get_cpu_stats options
  prev_total = options.delete(:total)
  prev_work = options.delete(:work)
  prev_total ||= 0
  prev_work ||= 0
  # Make this useful
  raw_stats = File.open("/proc/stat", "r").gets
  split_stats = raw_stats.split[1..-1].map(&:to_i)
  user, nice, system = split_stats.first(3)
  work_cycles = user + nice + system
  total_cycles = split_stats.reduce{|a,b| a + b}
  delta_total_cycles = total_cycles - prev_total
  delta_work_cycles = work_cycles - prev_work
  percent_cpu = delta_work_cycles / delta_total_cycles.to_f * 100
  @cpu = {total:total_cycles, work:work_cycles}
  color = case percent_cpu
          when 80 .. 100
            "red"
          when 40 ... 80
            "yellow"
          when 0 ... 40
            "green"
          end
  cpu_string = " CPU: %0.2f%" % percent_cpu
  cpu_string.fg(color)
end

def get_mem_status
  # Make this useful
  # /proc/meminfo
  stats = file_to_hash "/proc/meminfo"
  # all nums in kB
  total = stats[:memtotal].split(" ").first.to_f
  free = stats[:memfree].split(" ").first.to_f
  buffers = stats[:buffers].split(" ").first.to_f
  cached = stats[:cached].split(" ").first.to_f
  really_free = free + buffers + cached
  used = total - really_free
  percent_used = used / total * 100
  color = case percent_used
          when 80 .. 100
            "red"
          when 40 ... 80
            "yellow"
          when 0 ... 40
            "green"
          end
  mem_string = " MEM: %0.2f%" %  percent_used
  mem_string.fg(color)
end

def display
  #dzen options
  dzen = "dzen2 -p -y -1 "
  display_settings = "-xs 1 -ta l"
  actions = " -e 'onstart=lower;button4=scrollup;button5=scrolldown;'"
  font = " -fn 'Terminus-14'"

  #mpd setup
  @mpd = MPD.new
  begin
    @mpd.connect true
  rescue Exception
    puts "ERROR: MPD Connection fail"
  end

  #I herd you like pipes ...
  IO.popen dzen + display_settings + actions + font, "w" do |pipe|
    while true
      cpu = get_cpu_stats @cpu
      display = time_status + cpu + get_mem_status + get_mpd_status(@mpd) + bat_display + volume_display
      pipe.puts display
      sleep 0.70
    end
  end

end

# only start dzen if not called thru require
if  __FILE__ == $0
 display
end
