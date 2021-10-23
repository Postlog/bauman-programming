package main

import (
	"bytes"
	"github.com/IzeBerg/rss-parser-go"
	template2 "html/template"
	"net/http"
)

func rssHandler(writer http.ResponseWriter, request *http.Request) {
	rssObject, _ := rss.ParseRSS("https://lenta.ru/rss") // http://blagnews.ru/rss_vk.xml
	index, _ := template2.ParseFiles("./0_3/static/index.html")
	template, _ := template2.ParseFiles("./0_3/static/template.html")

	title := rssObject.Channel.Title
	count := len(rssObject.Channel.Items)
	var postsString string

	for v := range rssObject.Channel.Items {
		var buffer bytes.Buffer

		item := rssObject.Channel.Items[v]
		postTitle := item.Title
		postLink := item.Link
		postDescription := item.Description

		template.Execute(&buffer, struct {
			PostTitle       string
			PostLink        string
			PostDescription template2.HTML
		}{
			postTitle, postLink, template2.HTML(postDescription),
		})

		postsString += buffer.String()
	}

	index.Execute(writer, struct {
		Title      string
		PostsCount int
		Posts      template2.HTML
	}{
		title, count, template2.HTML(postsString),
	})
}

func main() {
	http.HandleFunc("/", rssHandler)
	http.ListenAndServe(":9000", nil)
}
