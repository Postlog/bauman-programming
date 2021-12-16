package RPCAPI

import "lab5/peer"

type API struct {
	peer *peer.Peer
}

type Messages struct {
	Messages []peer.Message
}

func NewAPI(peer *peer.Peer) *API {
	return &API{peer}
}

func (api *API) ConnectToNextPeer(_ interface{}, _ *interface{}) error {
	return api.peer.ConnectToNextPeer()
}

func (api *API) WriteMessageToNextPeer(text string, _ *interface{}) error {
	return api.peer.WriteMessageToNext(text)
}

func (api *API) Close(_ interface{}, _ *interface{}) error {
	return api.peer.Close()
}

func (api *API) GetMessages(_ interface{}, msg *Messages) error {
	*msg = Messages{api.peer.GetNewMessages()}
	return nil
}
