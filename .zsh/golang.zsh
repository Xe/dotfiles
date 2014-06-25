if [ -d $HOME/go ]
then
else
	mkdir -p $HOME/go/bin
fi

export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
