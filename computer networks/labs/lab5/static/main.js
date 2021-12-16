const connectionStatus = document.getElementById('connection-status__status')
const sendingStatus = document.getElementById('sending-status')
const connectButton = document.getElementById('connect-button')
const sendButton = document.getElementById('send-button')
const textarea = document.getElementById('text')
const messages = document.getElementById('messages')

const status = {
    connection: {
        connecting: 'connecting...',
        connected: 'connected',
        notConnected: 'not connected',
        notConnectedDetails: 'not connected, details in console'
    },
    sending: {
        processingSending: 'Sending...',
        sent: '',
        error: 'Sending error, details in console'
    }
}

let connected = false
function isConnected() {
    return connected
}

function setConnected(isConnected) {
    sendButton.disabled = !isConnected
    connected = isConnected
}

function rpcRequest(method, params = []) {
    return fetch('/rpc', {
        method: 'post',
        body: JSON.stringify({
            'jsonrpc': '2.0',
            'id': '1',
            'method': method,
            'params': params
        })
    }).then(res => res.json())
}

function setConnectionStatus(status) {
    connectionStatus.textContent = status
}

function setSendingStatus(status) {
    sendingStatus.textContent = status
}

connectButton.addEventListener('click', () => {
    setConnectionStatus(status.connection.connecting)
    setConnected(false)
    rpcRequest('API.ConnectToNextPeer').then(res => {
        if (res.error !== null) {
            setConnectionStatus(status.connection.notConnectedDetails)
            console.error(res.error)
        } else {
            setConnected(true)
            setConnectionStatus(status.connection.connected)
        }
    })
})

sendButton.addEventListener('click', () => {
    const text = textarea.value
    if (text === "") {
        return
    }
    textarea.value = ""

    sendButton.disabled = true
    setSendingStatus(status.sending.processingSending)

    rpcRequest('API.WriteMessageToNextPeer', [text]).then(res => {
        if (res.error !== null) {
            setConnectionStatus(status.connection.notConnectedDetails)
            setSendingStatus(status.sending.error)
            console.error(res.error)
        } else {
            setSendingStatus(status.sending.sent)
        }
    }).finally(() => {
        sendButton.disabled = false
    })
})

const generateMessagesOutput = (() => {
    const receivedMessages = []
    return (messages) => {
        console.log(messages)
        receivedMessages.push(...messages)
        let out = ''
        receivedMessages.forEach(message => {
            const text = message.Text
            const sender = message.SenderAddr
            const ts = message.Timestamp

            const date = new Date(ts * 1000).toISOString()
            out += `[${date}][${sender}]: ${text}\n`
        })
        return out
    }
})();

(() => {
    let requestSent = false

    setInterval(() => {
        if (requestSent || !isConnected()) {
            return
        }

        requestSent = true
        rpcRequest('API.GetMessages').then(res => {
            const newMessages = res.result.Messages
            if (newMessages.length === 0) {
                return
            }
            messages.innerText = generateMessagesOutput(newMessages)

        }).finally(() => {
            requestSent = false
        })
    }, 100)
})()
