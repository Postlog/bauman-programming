package main

import (
	"fmt"
	"github.com/jlaffaye/ftp"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"strings"
	"time"
)


func upload(connection *ftp.ServerConn) {
	var filePath, fileName string
	fmt.Print("Введите путь до файла: ")
	fmt.Scan(&filePath)

	file, err := os.Open(filePath)

	if err != nil {
		fmt.Println("Не удалось найти указанный файл")
		return
	}

	fmt.Print("Введите путь до файла для сохранения: ")
	fmt.Scan(&fileName)

	if err = connection.Stor(fileName, file); err != nil {
		fmt.Println("Произошла ошибка во время загрузки:", err)
		return
	}

	fmt.Println("Успешно загружено")
}

func download(connection *ftp.ServerConn) {
	var filePath, folderName string
	fmt.Print("Введите путь до файла на сервере: ")
	fmt.Scan(&filePath)
	fmt.Print("Введите путь до папки, в которую нужно сохранить файл: ")
	fmt.Scan(&folderName)


	if info, err := os.Stat(folderName); err != nil {
		fmt.Println("Пути", folderName, "не существует")
		return
	} else if !info.IsDir() {
		fmt.Println("Нужно указать путь до папки")
		return
	}

	reader, err := connection.Retr(filePath)

	if err != nil {
		fmt.Println("Произошла ошибка во время открытия файла на сервере:", err)
		reader.Close()
		return
	}

	buffer, err := ioutil.ReadAll(reader)
	reader.Close()

	fileName := filepath.Base(filePath)
	fullSavePath, _ := filepath.Abs(path.Join(folderName, fileName))
	err = ioutil.WriteFile(fullSavePath, buffer, os.FileMode(os.O_CREATE|os.O_WRONLY|os.O_TRUNC))

	if err != nil {
		fmt.Println("Ошибка во время записи файла на диск:", err)
		return
	}

	fmt.Println("Успещно сохранено в файл: ", fullSavePath)
}

func makeDir(connection *ftp.ServerConn) {
	var directoryName string
	fmt.Print("Введите название директории: ")
	fmt.Scan(&directoryName)

	if err := connection.MakeDir(directoryName); err != nil {
		fmt.Println("Ошибка во время создания директории на сервере:", err)
		return
	}

	fmt.Println("Директория", directoryName, "успешно создана")
}

func removeDir(connection *ftp.ServerConn) {
	var directoryName string
	fmt.Print("Введите название директории: ")
	fmt.Scan(&directoryName)

	if err := connection.RemoveDirRecur(directoryName); err != nil {
		fmt.Println("Ошибка во время удаления директории на сервере:", err)
		return
	}

	fmt.Println("Директория", directoryName, "успешно удалена")
}

func listDir(connection *ftp.ServerConn) {
	var directoryName string
	fmt.Print("Введите название директории: ")
	fmt.Scan(&directoryName)

	entries, err := connection.List(directoryName)

	if err != nil {
		fmt.Println("Ошибка во время чтения директории на сервере:", err)
	}
	fmt.Println("Содержимое дериктории", directoryName)
	for _, entry := range entries {
		fmt.Print("\t")
		switch entry.Type {
		case ftp.EntryTypeFile:
			fmt.Print("Файл: ")
		case ftp.EntryTypeFolder:
			fmt.Print("Папка: ")
		default:
			fmt.Print("Объект: ")
		}
		fmt.Println(entry.Name)
	}
}

func loop(connection *ftp.ServerConn) {
	defer connection.Quit()

	for {
		var command string
		fmt.Print("Введите комманду: ")
		fmt.Scan(&command)

		switch strings.ToLower(command) {
		case "upload":
			upload(connection)
		case "download":
			download(connection)
		case "makedir":
			makeDir(connection)
		case "removedir":
			removeDir(connection)
		case "listdir":
			listDir(connection)
		case "exit":
			if err := connection.Quit(); err != nil {
				fmt.Println("Во время отключения произошла ошибка:", err)
			} else {
				fmt.Println("Успешное отключение")
			}

			return
		default:
			fmt.Println("Неизвестная комманда")
		}
	}
}

func main() {
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
		return
	}

	if err = connection.Login(login, password); err != nil {
		fmt.Println("Ошибка авторизации:", err)
		return
	}

	fmt.Println("Успешное подключение")

	loop(connection)
}