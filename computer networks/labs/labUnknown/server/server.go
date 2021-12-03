package main

import (
	"encoding/json"
	"fmt"
	"github.com/gorilla/websocket"
	"io/ioutil"
	"net/http"
	"net/url"
	"strconv"
	"utils/common/logger"
)

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

const (
	Ping                  = 0
	Trace                 = 1
	ApplicationServerPort = "2228"
	Port                  = "1337"
)

type ActionInfo struct {
	ID      int
	Payload ActionInfoPayload
}

type ActionInfoPayload struct {
	URL  string
	Type string
}

type ServerResponse struct {
	ID      int    `json:"id"`
	Payload string `json:"payload"`
}

type Connection struct {
	Conn   *websocket.Conn
	Closed bool
}

func closeAndLog(connection *Connection, message string) {
	if connection.Closed {
		return
	}

	conn := connection.Conn

	logger.Warn(conn.RemoteAddr().String(), message)
	err := conn.Close()

	if err != nil {
		logger.Warn(conn.RemoteAddr().String(), "Ошибка во время закрытия WS-соединения: "+err.Error())
	}
}

func runApplication(actionType int, parsedURL *url.URL) (string, bool) {
	applicationServerURL := fmt.Sprintf(
		"http://127.0.0.1:%s?type=%d&url=%s",
		ApplicationServerPort,
		actionType,
		parsedURL.String(),
	)

	resp, err := http.Get(applicationServerURL)

	if err != nil {
		return "", false
	}

	result, _ := ioutil.ReadAll(resp.Body)

	return string(result), true
}

func makeApplicationRequest(Connection *Connection, messageID int, actionType int, parsedURL *url.URL) {
	result, ok := runApplication(actionType, parsedURL)

	if !ok {
		closeAndLog(Connection, "Ошибка приложения")
		return
	}

	response := ServerResponse{messageID, result}

	marshalled, _ := json.Marshal(&response)

	conn := Connection.Conn

	if err := conn.WriteMessage(websocket.TextMessage, marshalled); err != nil {
		closeAndLog(Connection, "Ошибка отправки сообщения клиенту: "+err.Error())
		return
	}

	logger.Info(conn.RemoteAddr().String(), "Ответ клиенту успешно отправлен")
}

func worker(conn *websocket.Conn) {
	connection := &Connection{conn, false}

	conn.SetCloseHandler(func(code int, text string) error {
		logger.Info(conn.RemoteAddr().String(), "Подключение закрыто")
		connection.Closed = true

		return nil
	})

	for !connection.Closed {
		messageType, data, err := conn.ReadMessage()

		if messageType != websocket.TextMessage {
			closeAndLog(connection, "Клиент передал сообщение в неподдерживаемом формате: "+strconv.Itoa(messageType))
			return
		}

		logger.Info(conn.RemoteAddr().String(), "Новое сообщение от клиента: "+string(data))

		if err != nil {
			closeAndLog(connection, "Ошибка чтения сообщения: "+err.Error())
			return
		}

		var actionInfo ActionInfo

		err = json.Unmarshal(data, &actionInfo)

		if err != nil {
			closeAndLog(connection, "Клиент отправил сообщение в некорректном формате")
			return
		}

		if actionInfo.Payload.Type != "ping" && actionInfo.Payload.Type != "trace" {
			closeAndLog(connection, "Клиент передал некорректный type")
			return
		}

		parsedURL, err := url.Parse(actionInfo.Payload.URL)

		if err != nil {
			closeAndLog(connection, "Клиент передал некорректный URL")
			return
		}

		var actionType int

		if actionInfo.Payload.Type == "ping" {
			actionType = Ping
		} else {
			actionType = Trace
		}

		go makeApplicationRequest(connection, actionInfo.ID, actionType, parsedURL)
	}
}

func handle(w http.ResponseWriter, r *http.Request) {
	conn, _ := upgrader.Upgrade(w, r, nil)
	logger.Info(r.RemoteAddr, "Новое WS-подключение")

	worker(conn)
}

func main() {
	logger.InitDefault()

	http.Handle("/", http.FileServer(http.Dir("./static")))
	http.HandleFunc("/ws", handle)

	err := http.ListenAndServe(":"+Port, nil)
	if err != nil {
		panic("Ошибка: " + err.Error())
	}
}
