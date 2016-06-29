#!/usr/bin/env ruby

# from http://errtheblog.com/posts/89-huba-huba
# modified to override existing files

def install_file(file)
  home = File.expand_path('~')
  target = File.join(home, ".#{file}")
  print `rm -rf #{target}\n`
  print `ln -s #{File.expand_path file} #{target}\n`
end

Dir['*'].each do |file|
  next if file =~ /install|live-packs/
  if file =~ /emacs.d/
    Dir["#{file}/*"].each do |file|
      install_file(file)
    end
    next
  end
  install_file(file)

end

# git push on commit
# `echo 'git push' > .git/hooks/post-commit`
# `chmod 755 .git/hooks/post-commit`
