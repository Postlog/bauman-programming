#!/bin/bash

function error {
	if [ $# -gt 0 ]
	then
		echo $1 1>&2
	fi
	exit 1
}

function isnumber {
	if ! [ $# -eq 1 ]
	then
		error "Функция isnumber принимает 1 обязательный аргумент, но было передано 0"
	else
		if [ $1 -eq $1 ] 2> /dev/null
		then
			return 0
		else
			return 1
		fi
	fi
}

function parsearguments {
	while [ $# -gt 0 ]; do
		case $1 in
			-t|--timeout)
			shift
			if [ $# -eq 0 ]; then
				error "Не указано значение параметра timeout"
			fi
			TIMEOUT=$1
			if ! isnumber $TIMEOUT; then
				error "Аргумент timeout должен являться числом"
			fi
			shift
			;;
			-p |--path)
			shift
			if [ $# -eq 0 ]; then
				error "Не указано значение параметра path"
			fi
			SCRIPT_PATH=$1

			if ! [ -f $SCRIPT_PATH ]; then
				error "Не удается найти указанный файл $SCRIPT_PATH"
			fi
			shift 
			;;
			*)
			error "Незивестный аргумент $1"
			;;
		esac
	done

	if [ -z $TIMEOUT ]; then
		error "Требуется указать аргумент timeout"
	fi

	if [ -z $SCRIPT_PATH ]; then
		error "Требуется указать аргумент path"
	fi
}

function main {
	PID=-1
	START_TIMESTAMP=$(date +%s)
	LOGS_FILE="output_$START_TIMESTAMP.log"
	ERRORS_FILE="errors_$START_TIMESTAMP.log"

	while [ 0 -eq 0 ]; do
		if ! ps -p $PID > /dev/null 2>&1; then
			bash $SCRIPT_PATH 1>$LOGS_FILE 2>$ERRORS_FILE & 
			PID=$!
			echo "Pid: $PID"
		else
			echo "Процесс не завершен, повторный запуск не произведен"
		fi
		sleep $TIMEOUT
	done
}

parsearguments $@
main