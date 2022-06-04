package main

import (
	"fmt"
	"sync"

	"github.com/go-ping/ping"
)

func worker(host string, count int) {
	pinger, err := ping.NewPinger(host)

	if err != nil {
		fmt.Println("Ошибка", err)
		return
	}

	pinger.Count = count
	pinger.SetPrivileged(true)

	pinger.OnRecv = func(packet *ping.Packet) {
		fmt.Printf("%d байт от %s: icmp_seq=%d time=%v\n", packet.Nbytes, packet.IPAddr, packet.Seq, packet.Rtt)
	}

	fmt.Printf("PING %s (%s):\n", pinger.Addr(), pinger.IPAddr())

	if err = pinger.Run(); err != nil {
		fmt.Println("Ошибка отправки PING-запроса", err)
	}
}

func main() {
	var (
		host         string
		count        int
		workersCount int
	)

	fmt.Print("Введите хост: ")
	fmt.Scan(&host)
	fmt.Print("Введите число пакетов: ")
	fmt.Scan(&count)
	fmt.Print("Введите число горутин: ")
	fmt.Scan(&workersCount)

	var wg sync.WaitGroup

	for i := 0; ; i++ {
		fmt.Println("Итерация номер", i)
		for j := 0; j < workersCount; j++ {
			wg.Add(1)

			go func() {
				worker(host, count)
				wg.Done()
			}()
		}
		wg.Wait()
	}
}
