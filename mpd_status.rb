#! /usr/bin/env ruby

def get_mpd_status
  mpd_status = `mpc status`
  status_array = mpd_status.split("\n")
  status = String.new
  if status_array.size == 3 && !status_array[1].include?("[paused]")
    @song_title = status_array[0].split("/").last
    song_status = status_array[1].split(" ")
    @song_status = song_status[0]
    @number_in_playlist = song_status[1]
    @song_seek = song_status[2]
    @song_percent_complete = song_status[3]
    daemon_status = status_array[2].split(":")
    @volume = daemon_status[1].split(" ").first
    @repeat = daemon_status[2].split(" ").first
    @random = daemon_status[3].split(" ").first
    status =  " MPD: #{@song_title} - #{@song_percent_complete} - VOL: #{@volume} - REP: #{@repeat} - RAN: #{@random}"
  else
    status = "MPD: NOT PLAYING"
  end
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

def bat_info_hash
  return file_to_hash "/proc/acpi/battery/BAT0/info"
end

def bat_state_hash
  return file_to_hash "/proc/acpi/battery/BAT0/state"
end

def bat_display
 status = " BAT: STATE: "
 state_hash = bat_state_hash
 info_hash = bat_info_hash
 status << state_hash[:charging_state]
 status << " PERCENT: "
 total = info_hash[:last_full_capacity].split(" ").first
 current = state_hash[:remaining_capacity].split(" ").first
 percent = ((current.to_f/total.to_f) * 100).round
 fg = case 
      when percent < 5
        "^fg(red)"
      when percent < 25
        "^fg(yellow)"
      else 
        "^fg(green)"
      end
 status << fg << percent.to_s << "% ^fg()" 
end



def chromo secs
  iArrColors = Array.new(60);
  iArrColors[0] = 0;
  iArrColors[1] = 4;
  iArrColors[2] = 8;
  iArrColors[3] = 13;
  iArrColors[4] = 17;
  iArrColors[5] = 21;
  iArrColors[6] = 26;
  iArrColors[7] = 30;
  iArrColors[8] = 34;
  iArrColors[9] = 38;
  iArrColors[10] = 43;
  iArrColors[11] = 47;
  iArrColors[12] = 51;
  iArrColors[13] = 55;
  iArrColors[14] = 60;
  iArrColors[15] = 64;
  iArrColors[16] = 68;
  iArrColors[17] = 72;
  iArrColors[18] = 76;
  iArrColors[19] = 81;
  iArrColors[20] = 85;
  iArrColors[21] = 89;
  iArrColors[22] = 93;
  iArrColors[23] = 98;
  iArrColors[24] = 102;
  iArrColors[25] = 106;
  iArrColors[26] = 110;
  iArrColors[27] = 115;
  iArrColors[28] = 119;
  iArrColors[29] = 123;
  iArrColors[30] = 128;
  iArrColors[31] = 132;
  iArrColors[32] = 136;
  iArrColors[33] = 140;
  iArrColors[34] = 145;
  iArrColors[35] = 149;
  iArrColors[36] = 153;
  iArrColors[37] = 157;
  iArrColors[38] = 161;
  iArrColors[39] = 166;
  iArrColors[40] = 170;
  iArrColors[41] = 174;
  iArrColors[42] = 178;
  iArrColors[43] = 183;
  iArrColors[44] = 187;
  iArrColors[45] = 191;
  iArrColors[46] = 195;
  iArrColors[47] = 200;
  iArrColors[48] = 204;
  iArrColors[49] = 208;
  iArrColors[50] = 213;
  iArrColors[51] = 217;
  iArrColors[52] = 221;
  iArrColors[53] = 225;
  iArrColors[54] = 230;
  iArrColors[55] = 234;
  iArrColors[56] = 238;
  iArrColors[57] = 242;
  iArrColors[58] = 247;
  iArrColors[59] = 251;
  iArrColors[60] = 255;


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
  fg = "^fg(#{ chromo((now.hour * 60) + now.min)})"
  now.strftime "DATE: #{fg} %A %B %d %I:%M:%S %p %Y ^fg()"
end




   

while true
  $stdout.flush
  display =   time_status + get_mpd_status + bat_display
  display = display.dump
  puts  display[1 ... display.size - 1]
  sleep 1
end


