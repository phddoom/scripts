#! /usr/bin/env ruby

COMMAND = "xrandr"

def query
  screen_regex = /(?:\s+)(\d+)(?:\s+)x(?:\s+)(\d+)/
  status = %x{#{COMMAND}}
  screen = status.lines.first
  minimum_match = /minimum#{screen_regex}/.match(screen)
  current_match = /current#{screen_regex}/.match(screen)
  maximum_match = /maximum#{screen_regex}/.match(screen)
  minimum_x, minimum_y = minimum_match.captures
  current_x, current_y = current_match.captures
  maximum_x, maximum_y = maximum_match.captures
  "minimum: #{minimum_x}x#{minimum_y} current: #{current_x}x#{current_y} maximum:#{maximum_x}x#{maximum_y}"
  monitor_regex = /(\w+)(?:\s*)(?:dis)?connected/
  monitors = []
  status.lines.each do |line|
    match = monitor_regex.match(line)
    if match
      monitor = match.captures.first
      monitors << monitor
    end
  end
  monitors.flatten.inspect
end

puts query
