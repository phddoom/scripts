class String
  def fg color
    "^fg(#{color})" + self + "^fg()"
  end

  def bg color
    "^bg(#{color})" + self + "^bg()"
  end

  def ca(button, action)
    "^ca(#{button},#{action})" + self + "^ca()"
  end

  def wrap_lines pix=20, width=500
    if self.size * pix > 500
      pix_count = 0
      words = self.split(" ")
      words.each_with_index do |word, index|
        if (pix_count + word.size * 20) > 500
          words[index - 1]=words[index - 1].strip + "\n"
          pix_count = 0
        else
          pix_count += (word.size * 20)
        end
      end
      return words.join " "
    else
      return self
    end
  end
end
