#!/bin/python3

import argparse
import re
import sys
from tokenize import tokenize


def load_dict(source):
	return source.read().split()

parser = argparse.ArgumentParser(add_help=False)
parser.add_argument('dict', metavar='<path_to_dictionary>', type=argparse.FileType('r'))
parser.add_argument('text', metavar='<path_to_text>', type=argparse.FileType('r'))
args = parser.parse_args()

if __name__ == '__main__':
	text_tokens = tokenize(args.text)
	dictionary = load_dict(args.dict)

	if len(dictionary) == 0:
		sys.stderr.write(f'{__file__}: словарь оказался пустым\n')
		sys.exit(1)

	wrong_words = {}
	for position, word in text_tokens.items():

		if word not in dictionary:
			wrong_words[position] = word

	if len(wrong_words) == 0:
		sys.exit(0)

	last_wrong_word_pos = list(wrong_words.keys())[-1]
	align = max(len(str(last_wrong_word_pos[0])), len(str(last_wrong_word_pos[1])))

	for position, word in wrong_words.items():
		line, column = position
		print(f'{line:>{align}}, {column:>{align}} {word}')