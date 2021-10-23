package proto

import "encoding/json"

// Request -- запрос клиента к серверу.
type Request struct {
	// Поле Command может принимать три значения:
	// * "quit" - прощание с сервером (после этого сервер рвёт соединение);
	// * "insert" - добавление элемента в массив по индексу
	// * "remove" - удаление элемента массива по индексу
	// * "sum" - вычисление суммы чисел на отрезке [startIndex; endIndex]
	Command string `json:"command"`

	// Если Command == "insert" или "remove", в поле Data должен лежать индекс
	// Если Command == "sum", в поле Data должны лежать начальный и конечный индексы
	// В противном случае поле Data пустое
	Data *json.RawMessage `json:"data"`
}

// Response -- ответ сервера клиенту.
type Response struct {
	// Поле Status может принимать три значения:
	// * "ok" - успешное выполнение запросов "push", "remove" или "quit"
	// * "result" - сумма на отрезке успешно вычислена
	// * "failed" - ошибка
	Status string `json:"status"`

	// Если Status == 2, то в поле Data хранится сообщение об ошибке
	// Если Status == 1, то в поле Data хранится значение суммы
	// В противном случае, поле Data пустое.
	Data *json.RawMessage `json:"data"`
}

// Sum -- сумма элементов массива
type Sum struct {
	Sum int64 `json:"sum"`
}

// ArrayIndex -- индекс массива.
type ArrayIndex struct {
	Index int `json:"index"`
}

// ArrayValue -- значение элемента массива
type ArrayValue struct {
	Value int `json:"value"`
}

// Range -- промежуток индексов массива
type Range struct {
	Start int `json:"start"`
	End int `json:"end"`
}
