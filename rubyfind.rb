#!/usr/bin/env ruby

STAT_METHODS = File::Stat.instance_methods - Object.instance_methods - Comparable.instance_methods - ["<=>"]

def find file
  dir = `gvim --remote-expr 'getcwd()'`.chomp
  dir ||= "~"
  Dir.chdir()
  results = []
  search_array = file.split("/")
  search_array[-1] = "*"+ search_array[-1]
  file = search_array.join("/")
  search_glob = "**/"+ (file.gsub(/(\w)/, '\1*').gsub("/", '/**/'))
  Dir.glob(search_glob) do |filename|
    stat = File.stat filename
    stat_hash = {"filename" => filename}
    STAT_METHODS.each do |key|
      stat_hash[key] = stat.method(key).call
    end
    results << stat_hash
  end
  results.sort do |a,b|
    b["atime"] <=> a["atime"]
  end
end

def get_input
  output = `zenity --entry --title="Search Current Gvim Dir" --text="Query:"`
  output.chomp
end

def display results
  results_array = []
  results.each do |file|
    results_array << file["filename"]
  end
  selection = ""
  IO.popen "dmenu -l 10 -p '#{results_array.size} files found'", "r+" do |pipe|
    pipe.puts results_array
    pipe.close_write
    selection = pipe.gets
  end
  return selection
end

 
`gvim --remote #{display(find(get_input))}`

