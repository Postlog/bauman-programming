#!/bin/python3

import argparse
import sys


def wc(files, params):
	totals = [0] * len(params)
	align = 8
	results = []
	for file in files:
		contents = file.read()
		results.append([contents.count('\n'),  	#  lines count
						len(contents.split()), 	#  words count
						len(contents), 			#  chars count 
						file.seek(0, 2)])       #  bytes count
	
		
		for i in range(len(totals)):
			totals[i] += results[-1][-(len(params) - i)]

	align = 0
	for total in totals:
		numbers_count = len(str(total))
		if numbers_count > align:
			align = numbers_count

	if not any(params):
		for i, result in enumerate(results):
			print(f'{result[0]:>{align}} {result[1]:>{align}} {result[3]:>{align}}', files[i].name)
		if len(files) > 1:
			print(f'{totals[0]:>{align}} {totals[1]:>{align}} {totals[3]:>{align}}', 'total')
	else:
		for file_i, result in enumerate(results):
			for i, result_value in enumerate(result):
				if params[i]:
					print(f'{result_value:>{align}}', end=' ')
			print(files[file_i].name)
		if len(files) > 1:
			for i, total in enumerate(totals):
				if params[i]:
					print(f'{total:>{align}}', end=' ')
			print('total')

def parse_unknowns(parser, unknowns):
	files = []
	for unknown in unknowns:
		try:
			files.append(open(unknown, 'r'))
		except:
			sys.stderr.write(f'{__file__}: не удается открыть указанный файл {unknown}\n')
	return files


parser = argparse.ArgumentParser(add_help=False)
parser.add_argument('-c', action='store_true')
parser.add_argument('-m', action='store_true')
parser.add_argument('-w', action='store_true')
parser.add_argument('-l', action='store_true')
args, unknowns = parser.parse_known_args()

files = parse_unknowns(parser, unknowns)

if __name__ == '__main__':
	data = None
	if not sys.stdin.isatty():
		data = sys.stdin

	if len(files) != 0:
		data = files

	if data is None:
		parser.error('Требуется указать путь до файа или передать текст в стандартный поток ввода')

	wc(data, [args.l, args.w, args.m, args.c])