#!/usr/bin/env ruby

load '/home/odin/scripts/dzen.rb'

class Notification
  attr_reader :title, :timeout, :urgency, :body
  def initialize(title, body, timeout = 5, urgency = false) 
    @title = title
    @body = body
    @timeout = timeout
    @urgency = urgency
  end
end

class NotificationHandler
  
  def initialize
    @notification_queue = Array.new
  end
  
  #Add to notification queue methods
  def queue notification
    @notification_queue.unshift notification
  end

  def << notification
    @notification_queue.unshift notification
  end

  #Methods that will dequeue or clear notification queue
  def notify
    notification = @notification_queue.shift
    return if notification.nil?
    num_of_lines, formatted_body = calculate_num_of_lines notification.body
    dzen = "dzen2 -p #{notification.timeout}"
    display_settings = " -geometry -500+0 -w 500 -l #{5} -ta l -sa c"
    actions = " -e 'onstart=uncollapse,scrollhome;button1=exit;button4=scrollup;button5=scrolldown;'"
    font = " -fn 'Terminus-8'"
    IO.popen dzen + display_settings + actions + font, "w" do |pipe|
      pipe.puts notification.title
      pipe.puts formatted_body
      pipe.close
    end
    self.notify
  end

  private
  
 
  #Will alter the body to break lines that
  #are too long to fit in the body width
  def calculate_num_of_lines body
    lines = body.split("\n")
    lines.each_with_index do |line, index|
      if line.length * 20 > 500
        lines[index] = line.wrap_lines
      end
    end
    reformated_body = lines.join "\n"
    num_lines = reformated_body.split("\n").size
    return num_lines, reformated_body
  end

end

#notification = Notification.new "This is a title", 
#  "This is a body with more than one line. Caused by one execeding long line that will be formated."
#handler = NotificationHandler.new 
#handler << notification
#handler.notify
