package main

import (
	"fmt"
	"github.com/mgutz/logxi/v1"
	"net/http"
	"net/url"
	"os/exec"
	"strconv"
	"utils/common/logger"
)

const Port = "2228"

type ActionInfo struct {
	URL  *url.URL
	Type int
}

func (info ActionInfo) String() string {
	return fmt.Sprintf("{Host: %s, Type: %d}", info.URL.Host, info.Type)
}

type Connection struct {
	Writer  http.ResponseWriter
	Request *http.Request
}

func respond(conn *Connection, statusCode int, message string) bool {
	conn.Writer.WriteHeader(statusCode)
	_, err := conn.Writer.Write([]byte(message))

	if err != nil {
		logger.Warn(conn.Request.RemoteAddr, "Не удалось отправить HTTP-ответ (%d) клиенту: %s", statusCode, message)
	}

	return err == nil
}

func respondBadRequest(
	conn *Connection,
	message string,
) {
	ok := respond(conn, 400, message)

	if ok {
		logger.Warn(conn.Request.RemoteAddr, message)
	}
}

func runCommand(command *exec.Cmd) (string, bool) {
	output, err := command.Output()

	if err != nil {
		return "", false
	}

	return string(output), true
}

func trace(parsedURL *url.URL) (string, bool) {
	return runCommand(exec.Command("traceroute", parsedURL.Host))
}

func ping(parsedURL *url.URL) (string, bool) {
	return runCommand(exec.Command("ping", "-c", "5", parsedURL.Host))
}

func parseURLParams(conn *Connection) (info ActionInfo, success bool) {
	query := conn.Request.URL.Query()

	urlParam := query.Get("url")
	typeParam := query.Get("type")
	success = true

	parsedURL, err := url.Parse(urlParam)

	if err != nil {
		success = false
	} else {
		info.URL = parsedURL
	}

	actionType, err := strconv.Atoi(typeParam)

	if err != nil {
		success = false
	} else {
		info.Type = actionType
	}

	return info, success
}

func handler(w http.ResponseWriter, r *http.Request) {
	conn := &Connection{w, r}

	if r.Method != http.MethodGet {
		respondBadRequest(conn, "Сервер принимает только GET-запросы")
		return
	}

	actionInfo, ok := parseURLParams(conn)

	if !ok {
		respondBadRequest(conn, "Клиент предоставил некорректные параметры запроса")
		return
	}

	logger.Info(r.RemoteAddr, "Новый запрос: %s", actionInfo)

	var result string
	switch actionInfo.Type {
	case 0:
		{
			result, ok = ping(actionInfo.URL)
		}
	case 1:
		{
			result, ok = trace(actionInfo.URL)
		}
	default:
		{
			respondBadRequest(conn, "Клиент указал некорректный actionType")
			return
		}
	}

	if !ok {
		respondBadRequest(conn, "Ошибка выполнения команды")
	} else {
		logger.Info(r.RemoteAddr, "Запрос %s успешно обработан, ответ:\n%s", actionInfo, result)
		respond(conn, 200, result)
	}
}

func main() {
	http.HandleFunc("/", handler)
	logger.SetLevel(log.LevelAll)

	if err := http.ListenAndServe(":"+Port, nil); err != nil {
		panic("Неожиданная ошибка: " + err.Error())
	}
}
