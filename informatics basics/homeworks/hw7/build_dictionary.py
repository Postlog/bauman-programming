#!/bin/python3

import argparse
import sys
from dict_utils import get_dictionary


parser = argparse.ArgumentParser(add_help=False)
parser.add_argument('-o', metavar='<output>', type=argparse.FileType('w+'))
parser.add_argument('source', metavar='<source_path>', type=argparse.FileType('r'), nargs='*')
args = parser.parse_args()

if __name__ == '__main__':
	data = None
	if not sys.stdin.isatty():
		data = sys.stdin

	if len(args.source) > 0:
		data = args.source

	if data is None:
		parser.error('Требуется указать путь до файа или передать текст в стандартный поток ввода')

	dictionary = get_dictionary(data)

	dictionary_representation = ''
	words = []
	for _, word in dictionary.items():
		if word not in words:
			dictionary_representation += f'{word}\n'
			words.append(word)
			
	if args.o:
		args.o.write(dictionary_representation)
	else:
		with open('dictionary_representation.txt', 'w+') as file:
			file.write(dictionary_representation)
