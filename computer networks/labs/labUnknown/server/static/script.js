const socket = new WebSocket('ws://localhost:1337/ws')
const output = document.getElementById('output')
const status = document.getElementById('status')

const form = document.getElementById('main-form')
const submitButton = document.getElementById('submit')

const urlInput = document.getElementById('url-input')
const radioInputPing = document.getElementById('radio-ping')
const responses = {}

status.innerText = 'Подключение...'
submitButton.disabled = true

function generateOutput() {
    let output = ''
    for (const [index, response] of Object.entries(responses).reverse()) {
        output += 'Запрос ' + index + '\n'
        if (response === null) {
            output += 'Обработка...\n\n'
        } else {
            output += '---------------------'
            output += response
            output += '---------------------\n\n'
        }
    }

    return output.trim()
}

function getIDGenerator() {
    let id = 1

    return () => {
        return id++
    }
}

const generateID = getIDGenerator()

function send(url, type) {
    const id = generateID()

    socket.send(JSON.stringify({
        id,
        payload: {url, type}
    }))

    return id
}


form.onsubmit = (event) => {
    event.preventDefault()

    const url = urlInput.value

    try {
        new URL(url)
    } catch (error) {
        return
    }

    const id = send(urlInput.value, radioInputPing.checked ? 'ping' : 'trace')

    responses[id] = null

    output.innerText = generateOutput()
}

socket.onopen = () => {
    status.innerText = 'Подключен'
    submitButton.disabled = false
}

socket.onmessage = (event) => {
    const message = event.data

    let data
    try {
        data = JSON.parse(message)
    } catch (exception) {
        console.error('Сервер ответил сообщением в неверном формате')
        socket.close()
        return
    }

    if (data.id === undefined || data.payload === undefined) {
        console.error('Сервер отправил некорректный ответ')
        socket.close()
        return
    }

    responses[data.id] = data.payload

    output.innerText = generateOutput()
}

socket.onclose = () => {
    console.error('Соединение закрыто')
    status.innerText = 'Соединение закрыто, перезагрузите страницу'
    submitButton.disabled = true
}

socket.onerror = () => {
    console.error('Произошла ошибка WS-соединения')
}