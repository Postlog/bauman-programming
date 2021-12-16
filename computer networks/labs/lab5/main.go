package main

import (
	"flag"
	"io"
	"lab5/RPCAPI"
	"lab5/log"
	"lab5/peer"
	"net/http"
	"net/rpc"
	"net/rpc/jsonrpc"
	"os"
	"os/signal"
)

type HTTPConn struct {
	r io.Reader
	w io.Writer
}

func (c *HTTPConn) Read(b []byte) (int, error) {
	return c.r.Read(b)
}

func (c *HTTPConn) Write(b []byte) (int, error) {
	return c.w.Write(b)
}

func (c *HTTPConn) Close() error {
	return nil
}

func GetRPCHandler(server *rpc.Server, logger log.Logger) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		codec := jsonrpc.NewServerCodec(&HTTPConn{r.Body, w})
		w.Header().Set("Content-type", "application/json")
		w.WriteHeader(200)
		err := server.ServeRequest(codec)
		if err != nil {
			logger.Errorf("Error during handling RPC request: %v", err)
			return
		}
	})
}

func main() {
	interfaceAddr := flag.String("webaddr", "127.0.0.1:8080", "address of web interface")
	selfAddr := flag.String("self", ":8000", "self address")
	nextAddr := flag.String("next", ":8001", "address of the next peer")
	flag.Parse()

	logger, err := log.New()
	if err != nil {
		panic(err)
	}

	p := peer.New(*selfAddr, *nextAddr, logger)

	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, os.Kill)
	go func() {
		<-c
		err = p.Close()
		if err != nil {
			logger.Errorf("Error occurred while closing peer: %v", err)
		}
		os.Exit(0)

	}()

	api := RPCAPI.NewAPI(p)
	server := rpc.NewServer()
	err = server.Register(api)
	if err != nil {
		panic(err)
	}

	http.Handle("/rpc", GetRPCHandler(server, logger))
	http.Handle("/", http.FileServer(http.Dir("./static")))
	go func() {
		err = p.ServeConnections()
		if err != nil {
			logger.Errorf("Error during serving connections: %v", err)
		}
	}()
	if err = http.ListenAndServe(*interfaceAddr, nil); err != nil {
		panic(err)
	}
}
