#!/bin/python3

import argparse
import sys
import re


def get_contents(textio):
	if type(textio) != list:
		textio = [textio]
		
	texts = {}
	for io in textio:
		texts[io.name] = io.read().strip()
	return texts


def grep(contents, string, ignore_case, display_line_number, regexp, max_count):
	prefix = ''
	if len(contents) > 1:
		prefix = '{filename}:'

	if display_line_number:
		prefix += '{line_number}:'
	prefix += ' ' * 4
	for name, text in contents.items():
		lines = text.split('\n')
		for i, line in enumerate(lines, 1):
			output = ''
			if regexp is not None:
				if len(re.findall(regexp, line)) != 0:
					output = prefix.format(filename=name, line_number=i) + line
			else:
				if ignore_case and string.lower() in line.lower() or string in line:
					output = prefix.format(filename=name, line_number=i) + line
			if output:
				print(output)



def parse_unknowns(parser, unknowns):
	files = []
	pattern = None
	for unknown in unknowns:
		try:
			files.append(open(unknown, 'r'))
		except:
			if pattern is None:
				pattern	= unknown
			else:
				sys.stderr.write(f'{__file__}: не удается открыть указанный файл {unknown}\n')
	return pattern, files


parser = argparse.ArgumentParser()
parser.add_argument('-i', dest='ignore_case', action='store_true')
parser.add_argument('-n', dest='line_number', action='store_true')
parser.add_argument('-e', dest='regexp', type=str)
parser.add_argument('-m', dest='max_count', type=int)

args, unknowns = parser.parse_known_args()
pattern, files = parse_unknowns(parser, unknowns)

if __name__ == '__main__':
	data = None
	if not sys.stdin.isatty():
		data = sys.stdin

	if len(files) != 0:
		data = files

	if data is None:
		parser.error('Требуется указать путь до файа или передать текст в стандартный поток ввода')

	if args.regexp is None:
		if pattern is None:
			parser.error('Требуется передать выражение для сопоставления')

	contents = get_contents(data)
	grep(contents, pattern, args.ignore_case, args.line_number, args.regexp, args.max_count)

