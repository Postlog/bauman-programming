package protocol

type Request struct {
	ID int `json:"id"`
	String string `json:"string"`
}

type Response struct {
	ID int `json:"id"`
	String string `json:"string"`
	Checksum uint64 `json:"checksum"`
}