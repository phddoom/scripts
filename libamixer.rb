class Mixer
  def initialize input
    input_arr = input.split "\n"
    @name, @card_no = parse_name_and_card input_arr[0]
    @capabilities = ""
    @channels = ""
    @limits = ""
  end

  def parse_name_and_card line
    line =~ /'(\w*)',(\d)/i
    return $1, $2.to_i
  end

  def parse_capabilities line
    
  end
end
