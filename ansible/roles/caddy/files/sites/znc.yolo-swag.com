znc.yolo-swag.com {
	log syslog
	tls xena@yolo-swag.com

	proxy / http://127.0.0.1:6668 {
		proxy_header Host {host}
	}
}
