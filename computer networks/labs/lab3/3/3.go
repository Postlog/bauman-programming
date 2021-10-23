package main

import (
	"fmt"
	"golang.org/x/net/trace"
	"net/http"
)
// A Fetcher fetches URL paths for a single domain.
type Fetcher struct {
	domain string
	events trace.EventLog
}

func NewFetcher(domain string) *Fetcher {
	return &Fetcher{
		domain,
		trace.NewEventLog("mypkg.Fetcher", domain),
	}
}

func (f *Fetcher) Fetch(path string) error {
	resp, err := http.Get(f.domain + "/" + path)
	if err != nil {
		f.events.Errorf("Get(%q) = %v", path, err)
		return err
	}
	f.events.Printf("Get(%q) = %s", path, resp.Status)
	
}

func (f *Fetcher) Close() error {
	f.events.Finish()
	return nil
}

func main() {
	f := NewFetcher("https://google.com")
	if err := f.Fetch("/"); err != nil {
		fmt.Println("Ошибка", err)
	}
}
