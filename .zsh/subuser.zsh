if [ -d ~/subuser ]
then
	PATH=$PATH:/home/xena/subuser/bin
fi

function install-subuser {
	git clone https://github.com/subuser-security/subuser ~/subuser
	PATH=$PATH:/home/xena/subuser/bin
}

