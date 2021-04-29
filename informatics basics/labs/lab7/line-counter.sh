#!/bin/bash

function error {
	if [ $# -gt 0 ]
	then
		echo $1 1>&2
	fi
	exit 1
}

if ! [ $# -eq 1 ]; then
	error "Требуется передать 1 аргумент (путь до проекта)"
fi

if ! [ -d "$1" ]; then
	error "Требуется указать путь до проекта, который является папкой"
fi

COUNT=0
while read f; do
	COUNT=$((COUNT + $(cat "$f" | sed '/^\s*$/d' | grep -c '')))
done <<< $(find "$1" -name "*.c" -o -name "*.h")
echo $COUNT