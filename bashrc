export PATH=$PATH:/Users/lborges/dev-softs/ant-1.8.0/bin
export PATH=$PATH:/opt/local/bin
export PATH=$PATH:/usr/local/mysql/bin
export PATH=$PATH:/usr/local/scala/bin
export PATH=$PATH:/usr/local/scala/bin
export EDITOR=mate
if [[ -s /Users/lborges/.rvm/scripts/rvm ]] ; then source /Users/lborges/.rvm/scripts/rvm ; fi

set -o vi

# Rails aliases
alias rc='rails console'
alias rg='rails generate'
alias rs='rails server'


# Git Aliases
alias gs='git status'
alias ga='git add'
alias gci='git commit'
alias gpom='git push origin master'
alias gphm='git push heroku master'
alias gdm='git diff | mate'
