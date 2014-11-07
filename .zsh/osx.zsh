# Don't use doored code

if [ "$(uname)" = "Darwin" ]; then
	export HOMEBREW_BUILD_FROM_SOURCE=1

	export DOCKER_HOST=tcp://192.168.59.103:2376
	export DOCKER_CERT_PATH=/Users/xena/.boot2docker/certs/boot2docker-vm
	export DOCKER_TLS_VERIFY=1
fi
