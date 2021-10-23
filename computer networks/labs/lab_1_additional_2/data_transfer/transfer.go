package main

import (
	"fmt"
	"github.com/jlaffaye/ftp"
	"path/filepath"
	"strings"
	"time"
)

func getTransferParticipants(connections []*ftp.ServerConn) (*ftp.ServerConn, *ftp.ServerConn, bool) {
	var indexA, indexB int
	fmt.Print("Введите индекс сервера-отправителя: ")
	fmt.Scan(&indexA)
	fmt.Print("Введите индекс сервера-получателя: ")
	fmt.Scan(&indexB)

	if indexA < 0 || indexA >= len(connections) || indexB < 0 || indexB > len(connections) {
		fmt.Println("Вы указали некорректный индекс")
		return nil, nil, false
	}

	return connections[indexA], connections[indexB], true
}

func download(connection *ftp.ServerConn) *ftp.Response {
	var filePath string
	fmt.Print("Введите путь до файла на сервере: ")
	fmt.Scan(&filePath)

	response, err := connection.Retr(filePath)

	if err != nil {
		fmt.Println("Произошла ошибка во время открытия файла на сервере:", err)
		return nil
	}

	return response
}

func upload(response *ftp.Response, connection *ftp.ServerConn) bool {
	var fileName string
	fmt.Print("Введите путь до файла для сохранения на сервере: ")
	fmt.Scan(&fileName)

	err := connection.MakeDir(filepath.Dir(fileName))
	if err != nil {
		fmt.Println("Не удалось создать нужную директорию для созранения")
		return false
	}

	if err = connection.Stor(fileName, response); err != nil {
		fmt.Println("Произошла ошибка во время загрузки:", err)
		return false
	}

	return true
}

func loop(connections []*ftp.ServerConn) {
	defer func() {
		for _, connection := range connections {
			connection.Quit()
		}
	}()

	for {
		var command string
		fmt.Print("Введите комманду: ")
		fmt.Scan(&command)

		switch strings.ToLower(command) {
		case "transfer":
			connectionA, connectionB, ok := getTransferParticipants(connections)

			if !ok {
				continue
			}

			response := download(connectionA)

			if response == nil {
				continue
			}

			if !upload(response, connectionB) {
				response.Close()
				continue
			}

			fmt.Println("Успешный трансфер")
		case "exit":
			for i, connection := range connections {
				if err := connection.Quit(); err != nil {
					fmt.Println("Во время отключения от сервера", i, "произошла ошибка:", err)
				} else {
					fmt.Println("Успешное отключение от сервера", i)
				}
			}
			return
		default:
			fmt.Println("Неизвестная комманда")
		}
	}
}

func getConnection() *ftp.ServerConn {
	var url, login, password string

	fmt.Print("Введите адрес: ")
	fmt.Scan(&url)
	fmt.Print("Введите имя пользователя: ")
	fmt.Scan(&login)
	fmt.Print("Введите пароль: ")
	fmt.Scan(&password)

	connection, err := ftp.Dial(url, ftp.DialWithTimeout(5 * time.Second))

	if err != nil {
		fmt.Println("Ошибка подключения:", err)
		return nil
	}

	if err = connection.Login(login, password); err != nil {
		fmt.Println("Ошибка авторизации:", err)
		return nil
	}

	return connection
}

func main() {
	var serversCount int
	fmt.Print("Введите число серверов: ")
	fmt.Scan(&serversCount)
	var connections []*ftp.ServerConn
	for i := 0; i < serversCount; i++ {
		fmt.Printf("Сервер #%d\n", i)
		connection := getConnection()
		if connection == nil {
			return
		}
		connections = append(connections, connection)
	}

	loop(connections)
}