#!/usr/bin/env ruby

STAT_METHODS = File::Stat.instance_methods - Object.instance_methods - Comparable.instance_methods - ["<=>"]

def find file, dir="/home/odin"
  Dir.chdir(dir)
  results = []
  search_array = file.split("/")
  last_word = search_array.last
  search_array[-1] = "*"+ search_array[-1] unless search_array[-1][0] == 46
  file = search_array.join("/")
  search_glob = "**/"+ (file.gsub(/(\w)/, '\1*').gsub("/", '/**/'))
  puts search_glob.to_s
  Dir.glob(search_glob) do |filename|
    results << filename
  end
   results.sort do |a,b|
     if a.downcase.include?(last_word)
      -1
     elsif b.downcase.include?(last_word)
       1
     else
       0
     end
   end
end

def get_input
  output = `kdialog --inputbox "Query"`
  output.chomp
end

def display results
  results_array = []
  results.first(10).each do |file|
    results_array << file
    results_array << file
  end
  `kdialog --menu "Select File:" #{results_array.join(" ")}`
end

`kdialog --msgbox #{ARGV.inspect}}`
input = get_input
if ARGV.first
  results = find input, AGRV.first
`kdialog --msgbox #{results.inspect}}`
else
  results = find input
`kdialog --msgbox #{results}}`
end
selection = display results

`kate -u #{selection}`

