#! /usr/bin/env ruby
#
# This script is used to manage various utils that need to handled
# when moving from one physical location to another.
#
# Resources Managed:
# * screens ----- xrandr
# * wallpapers -- nitrogen
# * status bars - dzen2, pkill

# Clamp is an awesome option parsing lib
require "clamp"
# Yaml lib for storing state and configs
require "yaml"
# Pesky spaces and other bad chars
require "shellwords"

class Monitor < Clamp::Command

  # Monitor subcommand to adjust the montior layout for a given location.
  # This subcommand relies on the xrandr executable.
  option ["-l","--list"], :flag, "List configured locations"
  parameter "LOCATION", "change layout for given location"
  def execute
    puts "Stopping status bars"
    StatusBar::stop
    puts "Adjusting monitor layout with xrandr"
    case location.to_sym
    when :laptop
      puts "Adjusting to laptop layout"
      `xrandr --output VGA-0 --off --output HDMI-0 --off --output LVDS --auto`
    when :home
      puts "Adjusting to home layout"
      `xrandr --output LVDS --off --output VGA-0 --auto`
    when :work
      puts "Adjusting to work layout"
      `xrandr --output LVDS --auto --right-of HDMI-0 --output HDMI-0 --auto --primary`
    else
      `xrandr --auto`
    end
    puts "Starting status bars"
    StatusBar::start
    puts "Adjusting complete"
  end
end

class StatusBar < Clamp::Command
  def self.start
    pid = spawn "mpd_status.rb"
    File.open "/home/odin/.status_bar.pid", "w+" do |file|
      file.puts pid.to_s
    end
  end
  subcommand "start", "Start your status bar" do
    parameter "[COMMAND]", "Specify command to run as status bar"
    def execute
      StatusBar::start
    end
  end
  def self.stop
    `pkill -P \`cat /home/odin/.status_bar.pid\``
  end
  subcommand "stop", "Stop your status bar" do
    def execute
      StatusBar::stop
    end
  end
end

class Wallpaper < Clamp::Command
  option ["-r", "--rotate"], :flag, "rotate wallpapers in the directory"
  parameter "FILE_OR_DIRECTORY ...", "file(s) or directories containg wallpapers"

  def execute
    config = YAML.load_file "/home/odin/.wallpaper_count"
    if rotate?
      Dir.chdir file_or_directory_list.first do
        wallpapers = Dir["**/*.{jpg,png}"]
        count = config[:count].to_i rescue 0
        # Make sure that the count is valid for the specified directory
        count = 0 if count > wallpapers.size - 1
        `nitrogen --set-zoom #{wallpapers[count].shellescape}`
        config[:count] = count + 1
        File.open "/home/odin/.wallpaper_count", "w" do |file|
          YAML.dump(config, file)
        end
      end
    end
  end
end

class Command < Clamp::Command
  subcommand "monitor", "adjust the monitor layout", Monitor
  subcommand "status", "adjust the status bar", StatusBar
  subcommand "wallpaper", "adjust wallpaper settings", Wallpaper
end


# only run command if run from shell
if  __FILE__ == $0
  Command.run
end
