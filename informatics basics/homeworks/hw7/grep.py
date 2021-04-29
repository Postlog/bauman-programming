#!/bin/python3

import argparse
import sys
import re


def get_contents(streams):
	contents = {}
	for stream in streams:
		contents[stream.name] = stream.read().strip()
	return contents


def grep(contents, pattern, ignore_case, display_line_number, regexp, max_count):
	prefix = ''
	if len(contents) > 1:
		prefix = '{filename}:'

	if display_line_number:
		prefix += '{line_number}:'

	prefix += ' ' * 4

	for name, text in contents.items():
		lines = text.split('\n')
		count = 0

		for i, line in enumerate(lines, 1):

			if max_count is not None and count == max_count:
				break

			output = ''
			if regexp is not None:
				if re.findall(regexp, line):
					output = prefix.format(filename=name, line_number=i) + line
			elif ignore_case and pattern.lower() in line.lower() or pattern in line:
				output = prefix.format(filename=name, line_number=i) + line

			if output:
				print(output)
				count += 1



def parse_unknowns(parser, unknowns):
	streams = []
	pattern = None
	for unknown in unknowns:
		try:
			streams.append(open(unknown, 'r'))
		except:
			if pattern is None:
				pattern	= unknown
			else:
				sys.stderr.write(f'{__file__}: не удается открыть указанный файл {unknown}\n')
	return pattern, streams


parser = argparse.ArgumentParser()
parser.add_argument('-i', dest='ignore_case', action='store_true')
parser.add_argument('-n', dest='line_number', action='store_true')
parser.add_argument('-e', dest='regexp', type=str)
parser.add_argument('-m', dest='max_count', type=int)

args, unknowns = parser.parse_known_args()


if __name__ == '__main__':
	pattern, streams = parse_unknowns(parser, unknowns)
 	
	if not sys.stdin.isatty() and len(streams) == 0:
		streams.append(sys.stdin)

	if len(streams) == 0:
		parser.error('Требуется указать путь до файа или передать текст в стандартный поток ввода')

	if args.regexp is None and pattern is None:
		parser.error('Требуется передать выражение для сопоставления')

	contents = get_contents(streams)
	grep(contents, pattern, args.ignore_case, args.line_number, args.regexp, args.max_count)
