package main

import (
	"fmt"
	"github.com/goftp/file-driver"
	"github.com/goftp/server"
)


func main() {
	factory := &filedriver.FileDriverFactory{
		RootPath: "./ftp/",
		Perm: server.NewSimplePerm("123", "123"),
	}

	ftpServer := server.NewServer(&server.ServerOpts{
		Factory: factory,
		Hostname: "127.0.0.1",
		Port:     21,
		Auth: &server.SimpleAuth{
			Name: "Postlog", Password: "Aboba",
		},
	})

	err := ftpServer.ListenAndServe()

	if err != nil {
		fmt.Println("Произошла ошибка во время запуска сервера:", err)
	}
}
