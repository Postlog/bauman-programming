package main

import (
	"fmt"
	"os"

	"github.com/go-ping/ping"
)

func main() {
	var host string
	var count int
	fmt.Print("Введите хост: ")
	fmt.Scan(&host)
	fmt.Print("Введите число пакетов: ")
	fmt.Scan(&count)
	pinger, err := ping.NewPinger(host)

	if err != nil {
		fmt.Println(err)
		fmt.Println("Не удалось подключиться к хосту", host)
		os.Exit(1)
	}
	pinger.SetPrivileged(true)
	pinger.Count = count

	pinger.OnRecv = func(packet *ping.Packet) {
		fmt.Printf("%d байт от %s: icmp_seq=%d time=%v\n", packet.Nbytes, packet.IPAddr, packet.Seq, packet.Rtt)
	}

	pinger.OnFinish = func(statistics *ping.Statistics) {
		fmt.Printf("\n--- %s Статистика ---\n", statistics.Addr)
		fmt.Printf("%d Отправлено, %d Получено, %v%% Потери\n", statistics.PacketsSent, statistics.PacketsRecv, statistics.PacketLoss)
		fmt.Printf("min/avg/max/stddev = %v/%v/%v/%v\n", statistics.MinRtt, statistics.AvgRtt, statistics.MaxRtt, statistics.StdDevRtt)
	}

	fmt.Printf("PING %s (%s):\n", pinger.Addr(), pinger.IPAddr())

	if err = pinger.Run(); err != nil {
		fmt.Println(err)
		fmt.Println("Не удалось подключиться к хосту", host)
	}
}
