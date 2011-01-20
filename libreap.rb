require 'mechanize'

SITE = "http://www.animecrazy.net"

class Anime
  attr_reader :name, :desc, :number_of_episodes

  def initialize name, number_of_episodes = nil, desc = nil
    @name = name
    @number_of_episodes = number_of_episodes || get_number_of_episodes
    @desc = desc || get_desc
  end

  def name_url
    @name.gsub(" ", "-")
  end

  private

  def get_number_of_episodes
    agent = Mechanize.new
    url = SITE + "/#{@name.gsub(" ", "-") + "-anime"}"
    anime_page = agent.get url
    anime_page.search(".epCount p").first.text =~ /Episodes: (\d*)/
    $1.to_i
  end

  def get_desc
    agent = Mechanize.new
    url = SITE + "/#{@name.gsub(" ", "-") + "-anime"}"
    anime_page = agent.get url
    anime_page.search(".desc").text.gsub("Description:", "").strip
  end

end

class Download
  attr_reader :anime, :type, :ep

  def initialize anime, ep = nil, link_regex = nil
    @anime = anime
    @ep = ep
    @link_regex = link_regex
    @type = case ep
            when Array, Range
              :multi
            else
              :single
            end
  end

  def reap
    case @type
    when :multi
      @ep.each do |ep|
        self.stage ep
        self.download
      end
    when :single
      self.stage @ep
      self.download
    else
      raise "Bad download type"
    end
  end

  def stage ep
    agent = Mechanize.new
    url = SITE + "/#{@anime.name_url + "-episode"}-#{ep}/"
    episode_page = agent.get url
    download_links = episode_page.links_with(:text => /download/i)
    download_links.delete_if{|link| link.text =~ /broken|\+/i}
    download_link = regex_or_user_input download_links
    download_link.attributes[:onclick] =~ /download\/(\d*)/
    @download_service_url = "http://www.animecrazy.net/mirrordownload/" + $1
  end

  def download
    agent = Mechanize.new
    download_page = agent.get @download_service_url
    megaupload_link = download_page.link_with(:href => /files/)
    puts megaupload_link.href
    puts "Getting Response ..."
    episode = agent.get URI.encode(megaupload_link.href, /\[|\]/)
    puts "Done"
    puts "Saving File to Disk ..."
    episode.save_as ""+ megaupload_link.href.split("/").last
    puts "Done"
    puts "Exiting"
  end

  def regex_or_user_input dls
    unless @link_regex.nil?
      regex_links = dls.select{|link| link.text =~ @link_regex}
      case regex_links.size
      when 0
        return user_input dls
      when 1
        return regex_links.first
      else
        return user_input regex_links
      end
    else
      user_input dls
    end
  end

  def user_input dls
    dls.each_with_index do |link, index|
      puts index.to_s + "\t" + link.text.strip
    end
    puts "Enter number of download link to use: "
    choice = STDIN.gets
    dls[choice.to_i]
  end

end

#########################################################################################
#
#OLD STUFF
#
#########################################################################################
def download anime, ep=nil
  puts "Anime: #{anime}"
  puts "Ep#: #{ep}" unless ep.nil?

  if ep.nil?
    max = get_number_of_ep anime
    puts "Total Number of Episodes for #{anime}: #{max}"
    latest = 0
    Dir.foreach(Dir.pwd) do |file|
      file =~ /\D*(\d*)/i
      current = $1.to_i
      latest = current if current > latest
    end
    puts "Latest Episode in Folder: #{latest}"
    (latest.succ .. max).each do |ep_num|
      download anime, ep_num
    end
  end

  agent = Mechanize.new
  url = SITE + "/#{anime.gsub(" ", "-") + "-episode"}-#{ep}/"
  episode_page = agent.get url
  puts "AnimeCrazy url: " + url
  download_links = episode_page.links_with(:text => /download/i)
  download_links.delete_if{|link| link.text =~ /broken|\+/i}
  download_links.each_with_index do |link, index|
    puts index.to_s + "\t" + link.text.strip
  end
  puts "Enter number of download link to use: "
  choice = STDIN.gets
  download_link = download_links[choice.to_i]
  download_link.attributes[:onclick] =~ /download\/(\d*)/
  true_download_link = "http://www.animecrazy.net/mirrordownload/" + $1
  puts true_download_link
  download_page = agent.get true_download_link
  megaupload_link = download_page.link_with(:href => /files/)
  puts megaupload_link.href
  puts "Getting Response ..."
  episode = agent.get URI.encode(megaupload_link.href, /\[|\]/)
  puts "Done"
  puts "Saving File to Disk ..."
  episode.save_as ""+ megaupload_link.href.split("/").last
  puts "Done"
  puts "Exiting"
end


