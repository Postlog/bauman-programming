package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"github.com/mgutz/logxi/v1"
	"hash/crc64"
	"lab4/protocol"
	"net"
)

const BufferSize = 1024*1024

func parseFlags() (conn *net.UDPConn, ok bool) {
	var (
		serverAddrStr string
		helpFlag      bool
	)

	flag.StringVar(&serverAddrStr, "addr", "127.0.0.1:6000", "set server IP address and port")
	flag.BoolVar(&helpFlag, "help", false, "print options list")

	if flag.Parse(); helpFlag {
		_ = fmt.Errorf("server [options]\n\nAvailable options:\n")
		flag.PrintDefaults()
	} else if serverAddr, err := net.ResolveUDPAddr("udp", serverAddrStr); err != nil {
		log.Error("resolving server address", "error", err)
	} else if conn, err = net.ListenUDP("udp", serverAddr); err != nil {
		log.Error("creating listening connection", "error", err)
	} else {
		ok = true
	}

	return
}

func main() {
	conn, ok := parseFlags()

	if !ok {
		return
	}

	log.Info("server listens incoming messages from clients")
	buf := make([]byte, BufferSize)

	for {
		if bytesRead, addr, err := conn.ReadFromUDP(buf); err != nil {
			log.Error("receiving message from client", "error", err)
		} else {
			var request protocol.Request

			err := json.Unmarshal(buf[:bytesRead], &request)

			if err != nil {
				log.Error("malformed request", "error", err)
				continue
			}

			s := request.String

			cs := crc64.Checksum([]byte(s), crc64.MakeTable(crc64.ISO))
			fmt.Println(cs)
			response := protocol.Response{ID: request.ID, Checksum: cs, String: s}

			responseBytes, err := json.Marshal(response)

			if err != nil {
				log.Error("error while marshalling response", "error", err)
				panic(err)
			}

			if _, err = conn.WriteToUDP(responseBytes, addr); err != nil {
				log.Error("sending message to client", "error", err, "client", addr.String())
			} else {
				log.Info("successful interaction with client", "string", request.String, "checksum", response.Checksum, "client", addr.String())
			}
		}
	}
}
