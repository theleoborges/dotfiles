export PATH=/usr/local/bin:$PATH
export PATH=~/libraries/android-sdk-macosx/tools:$PATH
export PATH=~/libraries/android-sdk-macosx/platform-tools:$PATH
export PATH=~/libraries/apache-ant-1.9.4/bin/:$PATH
export PATH=~/vert.x-2.1.5/bin/:$PATH

export PATH=~/bin:$PATH
export PATH=~/storm/bin:$PATH
export PATH=/Applications/Emacs.app/Contents/MacOS/bin:$PATH
export PATH=~/frameworks/scala-latest/bin:$PATH
export PATH=/usr/local/share/npm/bin:$PATH
export PATH=~/.cabal/bin:$PATH


export EDITOR=subl
export ARCHFLAGS='-arch x86_64'

# scala stuff
export SBT_OPTS="-XX:MaxPermSize=256M -Xmx1500M"
#export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_60.jdk/Contents/Home
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_60.jdk/Contents/Home

# rbenv
export RBENV_ROOT=/usr/local/var/rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

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



# Emacs aliases / functions
export ALTERNATE_EDITOR=""
export EDITOR=emacsclient
alias emacs='open -a /Applications/Emacs.app $1'
alias ed='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
alias edk="emacsclient -e '(kill-emacs)'"
# attach to running daemon
#alias ec='emacsclient -c $1 &'
# attach to running daemon, but in the terminal
alias et='emacsclient -nw $1'

function ec() { emacsclient -c "$@" & }

# Blog aliases
alias blog='cd ~/projects/ruby/octopress'
alias drafts='$EDITOR ~/projects/ruby/octopress/source/_drafts'

# Bash aliases
alias ls='ls -G'

#List zombie processes
alias zombies="ps -el | grep 'Z'"

#Check if port in use
alias portinuse='sudo lsof -i :$1'

# Put current Git branch in the terminal
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
export PS1="\[\033[00m\]\u@\h\[\033[01;34m\] \W \[\033[31m\]\$(parse_git_branch) \[\033[00m\]λ\[\033[00m\] "

# Prevents long prompts from garbling reverse searches
export TERM=xterm

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
export PATH=/usr/local/share/npm/bin:$PATH

### GPG key management
eval $(gpg-agent --daemon)


parse_git_branch_one() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}
