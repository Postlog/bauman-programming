module utils

go 1.15

replace utils/common/logger => ../common/logger

require (
	github.com/gorilla/websocket v1.4.2
	github.com/mattn/go-colorable v0.1.11 // indirect
	github.com/mgutz/ansi v0.0.0-20200706080929-d51e80ef957d // indirect
	github.com/mgutz/logxi v0.0.0-20161027140823-aebf8a7d67ab
	github.com/stretchr/testify v1.7.0 // indirect
)
