package main

import (
	"fmt"
	"github.com/goftp/file-driver"
	"github.com/goftp/server"
)

func main() {
	var rootPath, login, password string
	var port int
	fmt.Print("Введите путь до корневой директории FTP-сервера: ")
	fmt.Scan(&rootPath)
	fmt.Print("Укажите порт: ")
	fmt.Scan(&port)
	fmt.Print("Укажите запрашиваемое имя пользователя:")
	fmt.Scan(&login)
	fmt.Print("Укажите запрашиваемый пароль:")
	fmt.Scan(&password)

	factory := &filedriver.FileDriverFactory{
		RootPath: rootPath,
		Perm: server.NewSimplePerm("root", "root"), // ???
	}

	ftpServer := server.NewServer(&server.ServerOpts{
		Factory: factory,
		Hostname: "127.0.0.1",
		Port:     port,
		Auth: &server.SimpleAuth{
			Name: login, Password: password,
		},
	})

	err := ftpServer.ListenAndServe()

	if err != nil {
		fmt.Println("Произошла ошибка во время запуска сервера:", err)
	}
}
