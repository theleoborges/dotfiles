export PATH=/usr/local/bin:$PATH
export EDITOR=mate
export ARCHFLAGS='-arch x86_64'

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"


# Rails aliases
alias rc='rails console'
alias rg='rails generate'
alias rs='rails server'
alias rrollback='rake db:rollback RAILS_ENV=test && rake db:rollback'
alias rmigrate='rake db:migrate RAILS_ENV=test && rake db:migrate'
alias be='bundle exec'


# Git Aliases
alias gs='git status'
alias ga='git add'
alias gci='git commit'
alias gco='git checkout '
alias gp='git push'
alias gpo='git push origin'
alias gpom='git push origin master'
alias gphm='git push heroku master'
alias gdm='git diff | mate'
alias gsci='git show --pretty="format:" --name-only'

# Put current Git branch in the terminal
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
export PS1="\[\033[00m\]\u@\h\[\033[01;34m\] \W \[\033[31m\]\$(parse_git_branch) \[\033[00m\]$\[\033[00m\] "

# Prevents long prompts from garbling reverse searches
export TERM=xterm