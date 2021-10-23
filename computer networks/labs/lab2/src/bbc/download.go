package main

import (
	"github.com/mgutz/logxi/v1"
	"golang.org/x/net/html"
	"net/http"
	"strings"
)

type Item struct {
	Ref, Title, ImageSrc string
}

func getAttribute(node *html.Node, key string) string {
	for _, attr := range node.Attr {
		if attr.Key == key {
			return attr.Val
		}
	}
	return ""
}

func getChildren(node *html.Node) []*html.Node {
	var children []*html.Node
	for c := node.FirstChild; c != nil; c = c.NextSibling {
		children = append(children, c)
	}
	return children
}

func isElement(node *html.Node, tag string) bool {
	return node != nil && node.Type == html.ElementNode && node.Data == tag
}

func isTextNode(node *html.Node) bool {
	return node != nil && node.Type == html.TextNode
}

func classAttrContainsName(class string, target string) bool {
	for _, classname := range strings.Split(class, " ") {
		if classname == target {
			return true
		}
	}

	return false
}

func hasClass(node *html.Node, className string) bool {

	return classAttrContainsName(getAttribute(node, "class"), className)
}

func isDivNode(node *html.Node, classname string) bool {
	return isElement(node, "div") && classAttrContainsName(getAttribute(node, "class"), classname)
}

func getChildElement(node *html.Node, tag string) *html.Node {
	if node == nil {
		return nil
	}

	for child := node.FirstChild; child != nil; child = child.NextSibling {
		if isElement(child, tag) {
			return child
		}
	}

	return nil
}

//func readItem(item *html.Node) *Item {
//	if rootSpan := item.FirstChild; isElement(rootSpan, "span") {
//		if rootDiv := getChildElement(rootSpan, "div", "gs-o-media__body"); rootDiv != nil {
//			if a := rootDiv.FirstChild; isElement(a, "a") {
//				if span := a.FirstChild; isElement(span, "span") {
//					if children := getChildren(span); len(children) == 1 && isTextNode(children[0]) {
//						return &Item{
//							Ref:   getAttribute(a, "href"),
//							Title: children[0].Data,
//						}
//					}
//				}
//			}
//		}
//	}
//	return nil
//}

func getElementsByPredicate(node *html.Node, predicate func(*html.Node) bool) []*html.Node {
	var nodes []*html.Node

	for _, child := range getChildren(node) {
		if predicate(child) {
			nodes = append(nodes, child)
		}
		nodes = append(nodes, getElementsByPredicate(child, predicate)...)
	}

	return nodes
}

//func search(node *html.Node) []*Item {
//	if isDivNode(node, "nw-c-most-read__items") {
//		if container := getChildElement(node, "ol", "gel-layout__item"); container != nil {
//			var items []*Item
//			for _, child := range getChildren(container) {
//				if isElement(child, "li") {
//					if item := readItem(child); item != nil {
//						items = append(items, item)
//					}
//				}
//			}
//			return items
//		}
//	}
//
//	for c := node.FirstChild; c != nil; c = c.NextSibling {
//		if items := search(c); items != nil {
//			return items
//		}
//	}
//
//	return nil
//}

func readTitle(node *html.Node) string {
	if children := getChildren(node); len(children) == 1 && isTextNode(children[0]) {
		return children[0].Data
	}

	return ""
}

func readImage(node *html.Node) string {
	return getAttribute(node, "src")
}

func getTitle(doc *html.Node) string {
	nodes := getElementsByPredicate(doc, func(node *html.Node) bool {
		return getAttribute(node, "id") == "main-heading"
	})

	if len(nodes) == 0 {
		return ""
	}

	if children := getChildren(nodes[0]); len(children) == 1 && isTextNode(children[0]) {
		return children[0].Data
	}

	return ""
}

func getImageSrc(doc *html.Node) string {
	nodes := getElementsByPredicate(doc, func(node *html.Node) bool {
		return getAttribute(node, "data-component") == "image-block"
	})

	if len(nodes) == 0 {
		return ""
	}

	node := nodes[0]
	figure := getChildElement(node, "figure")
	div := getChildElement(figure, "div")
	span1 := getChildElement(div, "span")
	span2 := getChildElement(span1, "span")
	img := getChildElement(span2, "img")

	if img == nil {
		return ""
	}

	return getAttribute(img, "src")
}

func getItem(url string, doc *html.Node) *Item {
	return &Item{
		Ref:      url,
		Title:    getTitle(doc),
		ImageSrc: getImageSrc(doc),
	}
}

func getItems(nodes []*html.Node) []*Item {
	var items []*Item

	for _, node := range nodes {
		url := ROOT_URL + getAttribute(node, "href")
		doc := downloadHtml(url)
		items = append(items, getItem(url, doc))
	}

	return items
}

const ROOT_URL = "bbc.com"
const URL = "bbc.com/news"

func downloadHtml(url string) *html.Node {
	log.Info("sending request to " + url)
	if response, err := http.Get("https://" + url); err != nil {
		log.Error("request to "+url+" failed", "error", err)
	} else {
		defer response.Body.Close()
		status := response.StatusCode
		log.Info("got response from "+url, "status", status)
		if status == http.StatusOK {
			if doc, err := html.Parse(response.Body); err != nil {
				log.Error("invalid HTML from "+url, "error", err)
			} else {
				log.Info("HTML from " + url + " parsed successfully")

				return doc
			}
		}
	}

	return nil
}

func downloadNews() []*Item {
	log.Info("sending request to " + URL)
	if response, err := http.Get("https://" + URL); err != nil {
		log.Error("request to "+URL+" failed", "error", err)
	} else {
		defer response.Body.Close()
		status := response.StatusCode
		log.Info("got response from "+URL, "status", status)
		if status == http.StatusOK {
			if doc, err := html.Parse(response.Body); err != nil {
				log.Error("invalid HTML from "+URL, "error", err)
			} else {
				log.Info("HTML from " + URL + " parsed successfully")
				nodes := getElementsByPredicate(doc, func(node *html.Node) bool {
					return getAttribute(node, "class") == "gs-c-promo-heading nw-o-link gs-o-bullet__text gs-o-faux-block-link__overlay-link gel-pica-bold gs-u-pl-@xs"
				})

				return getItems(nodes)
			}
		}
	}
	return nil
}
