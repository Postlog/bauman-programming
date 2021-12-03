package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"github.com/mgutz/logxi/v1"
	"lab4/protocol"
	"net"
	"time"
)

func parseFlags() (conn *net.UDPConn, ok bool) {
	var (
		serverAddrStr string
		helpFlag      bool
	)

	flag.StringVar(&serverAddrStr, "server", "127.0.0.1:6000", "set server IP address and port")
	flag.BoolVar(&helpFlag, "help", false, "print options list")

	if flag.Parse(); helpFlag {
		_ = fmt.Errorf("client [options]\n\nAvailable options:\n")
		flag.PrintDefaults()
	} else if serverAddr, err := net.ResolveUDPAddr("udp", serverAddrStr); err != nil {
		log.Error("resolving server address", "error", err)
	} else if conn, err = net.DialUDP("udp", nil, serverAddr); err != nil {
		log.Error("creating connection to server", "error", err)
	} else {
		ok = true
	}

	return
}

var generateID = (func() func() int {
	counter := 0
	return func() int {
		counter++
		return counter
	}
})()

func write(conn *net.UDPConn, payload []byte) {
	conn.SetWriteDeadline(time.Now().Add(time.Second * 3))
	_, err := conn.Write(payload)

	for err != nil {
		log.Error("sending request to server", "error", err)
		conn.SetWriteDeadline(time.Now().Add(time.Second * 3))
		_, err = conn.Write(payload)
	}

	log.Info("request successfully sent")
}

func main() {
	conn, ok := parseFlags()

	if !ok {
		return
	}

	defer conn.Close()

	buf := make([]byte, 1024*1024)

	var currentID int

	for {
		var s string

		fmt.Print("Enter string: ")
		fmt.Scan(&s)

		id := generateID()

		request := protocol.Request{ID: id, String: s}

		requestBytes, err := json.Marshal(request)

		if err != nil {
			log.Error("cannot marshal request", "error", err)
			continue
		}

		currentID = request.ID
		write(conn, requestBytes)

		for {
			conn.SetReadDeadline(time.Now().Add(time.Second * 2))
			if bytesRead, err := conn.Read(buf); err != nil {
				log.Error("receiving answer from server", "error", err)
				write(conn, requestBytes)
			} else {
				var response protocol.Response
				err = json.Unmarshal(buf[:bytesRead], &response)

				if err != nil {
					log.Error("unexpected response schema", "error", err)
					break
				}

				if response.ID != currentID {
					log.Warn("duplicated response from server", "string", response.String, "checksum", response.Checksum)
				} else {
					log.Info("successful interaction with server", "string", response.String, "checksum", response.Checksum)
					fmt.Println("String:", response.String, ", checksum:", response.Checksum)
					break
				}
			}
		}
	}
}
