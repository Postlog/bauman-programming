module client

go 1.17

replace lab4/protocol => ../protocol

require (
	github.com/mgutz/logxi v0.0.0-20161027140823-aebf8a7d67ab
	lab4/protocol v0.0.0
)

require (
	github.com/mattn/go-colorable v0.1.12 // indirect
	github.com/mattn/go-isatty v0.0.14 // indirect
	github.com/mgutz/ansi v0.0.0-20200706080929-d51e80ef957d // indirect
	golang.org/x/sys v0.0.0-20210927094055-39ccf1dd6fa6 // indirect
)
