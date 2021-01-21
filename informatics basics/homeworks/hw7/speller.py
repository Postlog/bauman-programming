#!/bin/python3

import argparse
import sys
from dictionary_utils import get_dictionary


def load_dictionary(source):
	return source.read().split()


parser = argparse.ArgumentParser(add_help=False)
parser.add_argument('dictionary', metavar='<path_to_dictionary>', type=argparse.FileType('r'))
parser.add_argument('text', metavar='<path_to_text>', type=argparse.FileType('r'))
args = parser.parse_args()

if __name__ == '__main__':
	text_dictionary = get_dictionary(args.text)
	dictionary = load_dictionary(args.dictionary)

	if len(dictionary) == 0:
		sys.stderr.write(f'{__file__}: словарь оказался пустым\n')
		sys.exit(1)

	wrong_words = {}
	for position, word in text_dictionary.items():
		if word not in dictionary:
			wrong_words[position] = word

	if len(wrong_words) == 0:
		sys.exit(0)

	last_wrong_word_line, last_wrong_word_column = list(wrong_words.keys())[-1]
	align = max(len(str(last_wrong_word_line)),
				len(str(last_wrong_word_column)))

	for position, word in wrong_words.items():
		line, column = position
		print(f'{line:>{align}}, {column:>{align}} {word}')