package peer

import (
	"encoding/json"
	"errors"
	"net"
	"sync"
	"time"
	
	"github.com/postlog/bauman-programming/computer-networks/labs/lab5/log"
)

var (
	nextPeerNotConnErr = errors.New("next peer not connected")
	peerClosedErr      = errors.New("peer already closed")
)

type MessagesBuffer struct {
	buffer []Message
}

func (b *MessagesBuffer) Append(m Message) {
	b.buffer = append(b.buffer, m)
}

func (b *MessagesBuffer) Extract() []Message {
	cpy := make([]Message, len(b.buffer))
	copy(cpy, b.buffer)
	b.buffer = nil
	return cpy
}

type Message struct {
	Text, SenderAddr string
	Timestamp        int64
}

type Peer struct {
	AddrStr, NextAddrStr string
	Logger               log.Logger
	nextPeerConn         *net.TCPConn
	closed               bool
	messagesBuf          MessagesBuffer
	mutex                sync.Mutex
}

func New(addr, nextAddr string, logger log.Logger) *Peer {
	return &Peer{
		AddrStr:     addr,
		NextAddrStr: nextAddr,
		Logger:      logger,
		messagesBuf: MessagesBuffer{},
	}
}

func isTimeoutErr(err error) bool {
	netErr, ok := err.(net.Error)
	return ok && netErr.Timeout()
}

func (p *Peer) GetNewMessages() []Message {
	return p.messagesBuf.Extract()
}

func (p *Peer) ConnectToNextPeer() error {
	if p.isClosed() {
		return peerClosedErr
	}

	if !p.nextPeerClosed() {
		return nil
	}

	addr, err := net.ResolveTCPAddr("tcp", p.NextAddrStr)
	if err != nil {
		return err
	}
	conn, err := net.DialTCP("tcp", nil, addr)
	if err != nil {
		return err
	}
	p.Logger.Infof("Connected to the next peer %s", conn.LocalAddr())
	p.nextPeerConn = conn
	return nil
}

// WriteMessageToNext отправляет сообщение следующему пиру в кольце. В случае ошибки при отправке
// соединение закрывается
func (p *Peer) WriteMessageToNext(text string) error {
	if p.isClosed() {
		return peerClosedErr
	}

	if p.nextPeerClosed() {
		return nextPeerNotConnErr
	}

	msg := Message{
		Text:       text,
		SenderAddr: p.AddrStr,
		Timestamp:  time.Now().Unix(),
	}
	err := p.nextPeerConn.SetWriteDeadline(time.Now().Add(time.Second))
	if err != nil {
		_ = p.closeNextPeer()
		return err
	}
	enc := json.NewEncoder(p.nextPeerConn)
	err = enc.Encode(&msg)
	if err != nil {
		_ = p.closeNextPeer()
		return err
	}
	p.Logger.Infof("Message to the next peer (%s) successfully sent: %+v", p.nextPeerConn.LocalAddr(), msg)
	p.messagesBuf.Append(msg)
	return nil
}

func (p *Peer) ServeConnections() error {
	if p.isClosed() {
		return peerClosedErr
	}

	tcpAddr, err := net.ResolveTCPAddr("tcp", p.AddrStr)
	if err != nil {
		return nil
	}

	listener, err := net.ListenTCP("tcp", tcpAddr)
	if err != nil {
		return err
	}

	for !p.isClosed() {
		err = listener.SetDeadline(time.Now().Add(time.Millisecond * 100))
		if err != nil {
			_ = listener.Close()
			return err
		}

		if conn, err := listener.Accept(); err != nil {
			if isTimeoutErr(err) {
				continue
			}
			_ = listener.Close()
			return err
		} else {
			p.Logger.Infof("New connection accepted (%s)", conn.LocalAddr())
			err = p.receive(conn)
			if err != nil {
				p.Logger.Errorf("Error occurred during receiving messages from %s: %v", conn.LocalAddr(), err)
			}
		}
	}
	return nil
}

// Close освобождает ресурсы Peer
func (p *Peer) Close() error {
	p.mutex.Lock()
	defer p.mutex.Unlock()

	if p.closed {
		return peerClosedErr
	}

	p.closed = true

	if p.nextPeerConn != nil {
		err := p.nextPeerConn.Close()
		p.nextPeerConn = nil
		return err
	}
	return nil
}

// forwardMessageToNext пересылает сообщение следующему пиру в кольце. В случае ошибки соединение не
// закрывается
func (p *Peer) forwardMessageToNext(msg Message) error {
	if p.nextPeerClosed() {
		return nextPeerNotConnErr
	}

	err := p.nextPeerConn.SetWriteDeadline(time.Now().Add(time.Second))
	if err != nil {
		return err
	}
	enc := json.NewEncoder(p.nextPeerConn)
	err = enc.Encode(&msg)
	if err != nil {
		return err
	}
	return nil
}

func (p *Peer) receive(conn net.Conn) error {
	defer func() { _ = conn.Close() }()
	msg := Message{}
	for !p.isClosed() {
		err := conn.SetReadDeadline(time.Now().Add(time.Second))
		if err != nil {
			return err
		}
		dec := json.NewDecoder(conn)
		if err = dec.Decode(&msg); err != nil {
			if isTimeoutErr(err) {
				continue
			}
			return err
		}

		p.Logger.Infof("Received message form %s: %+v", conn.LocalAddr(), msg)

		if msg.SenderAddr != p.AddrStr {
			p.messagesBuf.Append(msg)
			if err = p.forwardMessageToNext(msg); err != nil {
				p.Logger.Warnf("Unable to forward message to the next peer: %v", err)
			}
		}
	}

	return nil
}

func (p *Peer) nextPeerClosed() bool {
	return p.nextPeerConn == nil
}

func (p *Peer) closeNextPeer() error {
	err := p.nextPeerConn.Close()
	p.nextPeerConn = nil
	return err
}

func (p *Peer) isClosed() bool {
	p.mutex.Lock()
	defer p.mutex.Unlock()
	return p.closed
}
