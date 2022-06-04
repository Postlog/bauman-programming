package main

import (
	"flag"
	"fmt"
	"github.com/mgutz/logxi/v1"
	"math/rand"
	"net"
	"os"
	"time"
)

type Mode int

const (
	ClientToServer Mode = 0
	ServerToClient      = 1
	DUP                 = 2
)

var failures = map[Mode]string{
	ClientToServer:       "sending message to server",
	ServerToClient:       "sending message to client",
	ClientToServer | DUP: "sending duplicate message to server",
	ServerToClient | DUP: "sending duplicate message to client",
}

var successes = map[Mode]string{
	ClientToServer:       "client => server",
	ServerToClient:       "server => client",
	ClientToServer | DUP: "client => server (duplicate)",
	ServerToClient | DUP: "server => client (duplicate)",
}

var drops = map[Mode]string{
	ClientToServer: "dropping message to server",
	ServerToClient: "dropping message to client",
}

func send(conn *net.UDPConn, addr *net.UDPAddr, data []byte, mode Mode) {
	if _, err := conn.WriteToUDP(data, addr); err != nil {
		log.Error(failures[mode], "error", err, "client", addr.String())
	} else {
		log.Info(successes[mode], "size", len(data), "client", addr.String())
	}
}

func buggySend(conn *net.UDPConn, addr *net.UDPAddr, data []byte, mode Mode, loss, dup uint) {
	if rand.Intn(100) < int(loss) {
		log.Info(drops[mode], "size", len(data), "client", addr.String())
	} else {
		send(conn, addr, data, mode)
		if rand.Intn(100) < int(dup) {
			dataCopy := make([]byte, len(data))
			copy(dataCopy, data)

			duration := time.Duration(rand.Intn(20)) * time.Millisecond
			time.AfterFunc(duration, func() {
				send(conn, addr, dataCopy, mode|DUP)
			})
		}
	}
}

func serveClient(proxyConn, conn *net.UDPConn, addr *net.UDPAddr, loss, dup uint) {
	addrStr := addr.String()
	log.Info("serving new client", "client", addrStr)

	buf := make([]byte, 65507)
	for {
		if bytesRead, err := conn.Read(buf); err != nil {
			log.Error("receiving message from server", "error", err, "client", addrStr)
		} else {
			buggySend(proxyConn, addr, buf[:bytesRead], ServerToClient, loss, dup)
		}
	}
}

func proxy(proxyAddr, serverAddr *net.UDPAddr, loss, dup uint) {
	nat := make(map[string]*net.UDPConn)
	if proxyConn, err := net.ListenUDP("udp", proxyAddr); err != nil {
		log.Error("creating listening connection for proxy", "error", err)
	} else {
		log.Info("proxy listens incoming messages from clients")

		buf := make([]byte, 65507)
		for {
			if bytesRead, clientAddr, err := proxyConn.ReadFromUDP(buf); err != nil {
				log.Error("receiving message from client", "error", err)
			} else {
				clientAddrStr := clientAddr.String()
				conn, found := nat[clientAddrStr]
				if !found {
					addr := &net.UDPAddr{IP: proxyAddr.IP, Port: 0}
					if conn, err = net.ListenUDP("udp", addr); err != nil {
						log.Error("creating connection to server", "error", err, "client", clientAddrStr)
						continue
					}
					nat[clientAddrStr] = conn
					go serveClient(proxyConn, conn, clientAddr, loss, dup)
				}
				buggySend(conn, serverAddr, buf[:bytesRead], ClientToServer, loss, dup)
			}
		}
	}
}

func main() {
	var (
		proxyAddrStr, serverAddrStr string
		loss, dup                   uint
		helpFlag                    bool
	)

	flag.StringVar(&proxyAddrStr, "addr", "127.0.0.1:6060", "set proxy IP address and port")
	flag.StringVar(&serverAddrStr, "server", "127.0.0.1:6000", "set server IP address and port")
	flag.UintVar(&loss, "loss", 0, "set datagram loss rate (in %)")
	flag.UintVar(&dup, "dup", 0, "set datagram duplication rate (in %)")
	flag.BoolVar(&helpFlag, "help", false, "print options list")

	if flag.Parse(); helpFlag {
		fmt.Fprint(os.Stderr, "proxy [options]\n\nAvailable options:\n")
		flag.PrintDefaults()
	} else if loss > 100 {
		log.Error("loss rate cannot be greater than 100")
	} else if dup > 100 {
		log.Error("duplication rate cannot be greater than 100")
	} else if proxyAddr, err := net.ResolveUDPAddr("udp", proxyAddrStr); err != nil {
		log.Error("resolving proxy address", "error", err)
	} else if serverAddr, err := net.ResolveUDPAddr("udp", serverAddrStr); err != nil {
		log.Error("resolving server address", "error", err)
	} else {
		proxy(proxyAddr, serverAddr, loss, dup)
	}
}
