require 'socket'
require 'io/wait'

class MPD
  attr_reader :socket

  def initialize
    @socket = TCPSocket::new "localhost", 6600
    @socket.sync = true
    puts @socket.gets
  end

  def send_command command
    @socket.puts command
    status = nil
    response = ""
    until status
      while @socket.ready?
        tmp = @socket.gets
        status = :OK if tmp =~ /OK/
        status = :ACK if tmp =~ /ACK/
        response << tmp
      end
    end
    parse_response response, status
  end

  def parse_response response, type
    case type
    when :OK
      response
    when :ACK
      response
    else
      response
    end
  end

end
