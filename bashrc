export PATH=/usr/local/bin:$PATH
export PATH=~/frameworks/android-sdk-macosx/tools:$PATH
export PATH=~/frameworks/android-sdk-macosx/platform-tools:$PATH
export PATH=~/bin:$PATH
export PATH=~/storm/bin:$PATH

export EDITOR=subl
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
export GIT_COMMAND=`which git`

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
alias gclean="git clean -f -d"
alias gpr="git pull --rebase"
alias gdf="git diff"
alias gdc="git diff --cached"
alias glg="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --"
function gitCommitHistorySearch() {
    $GIT_COMMAND grep $@ $(git rev-list --all)
}


# Blog aliases
alias blog='cd ~/projects/ruby/octopress'
alias drafts='$EDITOR ~/projects/ruby/octopress/source/_drafts'

#List zombie processes
alias zombies="ps -el | grep 'Z'"

# Put current Git branch in the terminal
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
export PS1="\[\033[00m\]\u@\h\[\033[01;34m\] \W \[\033[31m\]\$(parse_git_branch) \[\033[00m\]λ\[\033[00m\] "

# Prevents long prompts from garbling reverse searches
export TERM=xterm
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

export PATH=/Users/lborges/bin/Sencha/Cmd/3.0.0.250:$PATH

export SENCHA_CMD_3_0_0="/Users/lborges/bin/Sencha/Cmd/3.0.0.250"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
