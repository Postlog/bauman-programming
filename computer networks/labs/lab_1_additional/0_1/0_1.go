package main

import (
	"fmt"
	"net/http"
	"net/url"
)

func requestHandler(writer http.ResponseWriter, request *http.Request) {
	request.ParseForm()
	data, ok := request.Form["url"]
	if !ok || data[0] == "" {
		http.Redirect(writer, request, "/?error=true", 301)
	} else {
		url, err := url.Parse(data[0])

		if err != nil || url.Host == "" {
			http.Redirect(writer, request, "/?error=true", 301)
		} else {
			fmt.Println(url.Host)
			http.Redirect(writer, request, "/frame.html?url=" + url.Host, 301)
		}
	}
}

func staticHandler(writer http.ResponseWriter, request *http.Request) {
	path := request.URL.Path
	if path == "/" {
		path = "./0_1/static/index.html"
	} else {
		path += ".html"
	}

	http.ServeFile(writer, request, path)
}

func main()  {
	fmt.Println("Сервер запущен")
	http.Handle("/", http.FileServer(http.Dir("./0_1/static/")))
	http.HandleFunc("/request", requestHandler)
	err := http.ListenAndServe(":9000", nil)
	if err != nil {
		fmt.Println(err)
	}
}
