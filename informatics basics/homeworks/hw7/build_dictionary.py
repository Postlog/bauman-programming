#!/bin/python3

import argparse
import sys
from tokenize import tokenize


parser = argparse.ArgumentParser(add_help=False)
parser.add_argument('-o', metavar='output', type=argparse.FileType('w+'))
parser.add_argument('source', type=argparse.FileType('r'), nargs='*')
args = parser.parse_args()

if __name__ == '__main__':
	data = None
	if not sys.stdin.isatty():
		data = [sys.stdin]

	if len(args.source) > 0:
		data = args.source

	if data is None:
		parser.error('Требуется указать путь до файа или передать текст в стандартный поток ввода')

	tokens = tokenize(data)

	dictionary = ''
	words = []
	for position, word in tokens.items():
		if word not in words:
			dictionary += f'{word}\n'
			words.append(word)
	if args.o:
		args.o.write(dictionary)
	else:
		with open('dictionary.txt', 'w+') as file:
			file.write(dictionary)
