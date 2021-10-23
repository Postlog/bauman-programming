package main

import (
	"fmt"      // пакет для форматированного ввода вывода
	"github.com/IzeBerg/rss-parser-go"
	"log"      // пакет для логирования
	"net/http" // пакет для поддержки HTTP протокола
	"strings"  // пакет для работы с UTF-8 строками
)

func HomeRouterHandler(w http.ResponseWriter, r *http.Request) {
	r.ParseForm() //анализ аргументов,

	fmt.Println(r.Form) // ввод информации о форме на стороне сервера
	fmt.Println("path", r.URL.Path)
	fmt.Println("scheme", r.URL.Scheme)
	fmt.Println(r.Form["url_long"])

	for k, v := range r.Form {
		fmt.Println("key:", k)
		fmt.Println("val:", strings.Join(v, ""))
	}

	fmt.Fprintf(w, "Test!") // отправляем данные на клиентскую сторону
}

func main1() {
	http.HandleFunc("/", HomeRouterHandler) // установим роутер

	err := http.ListenAndServe(":9000", nil) // задаем слушать порт
	if err != nil {
		log.Fatal("ListenAndServe: ", err)
	}
}

func main() {

	rssObject, err := rss.ParseRSS("http://blagnews.ru/rss_vk.xml")

	if err == nil {
		fmt.Printf("Title : %s\n", rssObject.Channel.Title)
		fmt.Printf("Generator : %s\n", rssObject.Channel.Generator)
		fmt.Printf("PubDate : %s\n", rssObject.Channel.PubDate)
		fmt.Printf("LastBuildDate : %s\n", rssObject.Channel.LastBuildDate)
		fmt.Printf("Description : %s\n", rssObject.Channel.Description)
		fmt.Printf("Number of Items : %d\n", len(rssObject.Channel.Items))

		for v := range rssObject.Channel.Items {
			item := rssObject.Channel.Items[v]
			fmt.Println()
			fmt.Printf("Item Number : %d\n", v)
			fmt.Printf("Title : %s\n", item.Title)
			fmt.Printf("Link : %s\n", item.Link)
			fmt.Printf("Description : %s\n", item.Description)
			fmt.Printf("Guid : %s\n", item.Guid.Value)
		}
	}
}
