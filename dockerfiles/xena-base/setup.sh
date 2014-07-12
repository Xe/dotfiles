# Install oh my zsh
wget --no-check-certificate https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | bash

function setlink
{
        ln -s $HOME/code/dotfiles/$1 $HOME/$1
}

rm ~/.zshrc

#set links
setlink .profile
setlink .zshrc
setlink .zsh
setlink .vim
setlink .vimrc
setlink .cheat
setlink .gitconfig

# Setup vundle
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

vim +PluginInstall +qall

(cd ~/.vim/bundle/YouCompleteMe; ./install.sh --clang-completer)
(cd ~/.vim/bundle/vimproc.vim; make)

# Golang stuff
(mkdir -p ~/go/{pkg,bin,src})

echo "Set up!"
