package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"github.com/mgutz/logxi/v1"
	"net"
)

import "proto"

// Client - состояние клиента.
type Client struct {
	logger log.Logger    // Объект для печати логов
	conn   *net.TCPConn  // Объект TCP-соединения
	enc    *json.Encoder // Объект для кодирования и отправки сообщений
	array  []int
}

// NewClient - конструктор клиента, принимает в качестве параметра
// объект TCP-соединения.
func NewClient(conn *net.TCPConn) *Client {
	return &Client{
		logger: log.New(fmt.Sprintf("client %s", conn.RemoteAddr().String())),
		conn:   conn,
		enc:    json.NewEncoder(conn),
	}
}

// serve - метод, в котором реализован цикл взаимодействия с клиентом.
// Подразумевается, что метод serve будет вызаваться в отдельной go-программе.
func (client *Client) serve() {
	defer client.conn.Close()
	decoder := json.NewDecoder(client.conn)
	for {
		var req proto.Request
		if err := decoder.Decode(&req); err != nil {
			client.logger.Error("cannot decode message", "reason", err)
			break
		} else {
			client.logger.Info("received command", "command", req.Command)
			if client.handleRequest(&req) {
				client.logger.Info("shutting down connection")
				break
			}
		}
	}
}

// handleRequest - метод обработки запроса от клиента. Он возвращает true,
// если клиент передал команду "quit" и хочет завершить общение.
func (client *Client) handleRequest(req *proto.Request) bool {
	switch req.Command {
	case "quit":
		client.respond("ok", nil)
		return true
	case "push":
		errorMsg := ""
		if req.Data == nil {
			errorMsg = "data field is absent"
		} else {
			var value proto.ArrayValue
			if err := json.Unmarshal(*req.Data, &value); err != nil {
				errorMsg = "malformed data field"
			} else {
				client.logger.Info("performing pushing", "value", value.Value)
				client.array = append(client.array, value.Value)
			}
		}
		if errorMsg == "" {
			client.respond("ok", nil)
		} else {
			client.logger.Error("pushing failed", "reason", errorMsg)
			client.respond("failed", errorMsg)
		}
	case "remove":
		errorMsg := ""
		if req.Data == nil {
			errorMsg = "data field is absent"
		} else {
			var index proto.ArrayIndex
			if err := json.Unmarshal(*req.Data, &index); err != nil {
				errorMsg = "malformed data field"
			} else {
				if length := len(client.array); index.Index >= length || index.Index < 0 {
					errorMsg = fmt.Sprintf("Индекс не соответствует диапазону [0; %d]", length - 1)
				} else {
					client.logger.Info("performing removing", "index", index.Index)
					client.array = append(client.array[:index.Index], client.array[index.Index+1:]...)
				}
			}
		}
		if errorMsg == "" {
			client.respond("ok", nil)
		} else {
			client.logger.Error("removing failed", "reason", errorMsg)
			client.respond("failed", errorMsg)
		}
	case "sum":
		errorMsg := ""
		var sum int64
		if req.Data == nil {
			errorMsg = "data field is absent"
		} else {
			var indexRange proto.Range
			if err := json.Unmarshal(*req.Data, &indexRange); err != nil {
				errorMsg = "malformed data field"
			} else {
				length := len(client.array)
				if length == 0 {
					errorMsg = "Массив пустой"
				} else if indexRange.Start > indexRange.End || indexRange.Start < 0 || indexRange.End >= length {
					errorMsg = fmt.Sprintf("Диапазон не соответствует размерам массива [0; %d]", length - 1)
				} else {
					client.logger.Info("performing sum", "Start", indexRange.Start, "End", indexRange.End)
					sum = calculateSum(client.array, indexRange.Start, indexRange.End)
				}
			}
		}

		if errorMsg == "" {
			client.respond("result", &proto.Sum{sum})
		} else {
			client.logger.Error("failed", "reason", errorMsg)
			client.respond("failed", errorMsg)
		}
	default:
		client.logger.Error("unknown command")
		client.respond("failed", "unknown command")
	}
	return false
}

// respond - вспомогательный метод для передачи ответа с указанным статусом
// и данными. Данные могут быть пустыми (data == nil).
func (client *Client) respond(status string, data interface{}) {
	var raw json.RawMessage
	raw, _ = json.Marshal(data)
	client.enc.Encode(&proto.Response{status, &raw})
}

func calculateSum(slice []int, start, end int) int64 {
	var sum int64 = 0
	for i := start; i <= end; i++ {
		sum += int64(slice[i])
	}
	return sum
}

func main() {
	// Работа с командной строкой, в которой может указываться необязательный ключ -addr.
	var addrStr string
	flag.StringVar(&addrStr, "addr", "127.0.0.1:6000", "specify ip address and port")
	flag.Parse()

	// Разбор адреса, строковое представление которого находится в переменной addrStr.
	if addr, err := net.ResolveTCPAddr("tcp", addrStr); err != nil {
		log.Error("address resolution failed", "address", addrStr)
	} else {
		log.Info("resolved TCP address", "address", addr.String())

		// Инициация слушания сети на заданном адресе.
		if listener, err := net.ListenTCP("tcp", addr); err != nil {
			log.Error("listening failed", "reason", err)
		} else {
			// Цикл приёма входящих соединений.
			for {
				if conn, err := listener.AcceptTCP(); err != nil {
					log.Error("cannot accept connection", "reason", err)
				} else {
					log.Info("accepted connection", "address", conn.RemoteAddr().String())

					// Запуск go-программы для обслуживания клиентов.
					go NewClient(conn).serve()
				}
			}
		}
	}
}
