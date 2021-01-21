#!/bin/python3

import argparse
import sys


def _get_file_data(file):
	contents = file.read()
	return [contents.count('\n'),  	#  lines count
			len(contents.split()), 	#  words count
			len(contents), 			#  chars count 
			len(contents.encode('utf-8'))]     	#  bytes count


def _get_file_name(file):
	if hasattr(file, 'custom_name'):
		return file.custom_name
	else:
		return file.name


def wc(files, params):
	if type(files) != list:
		files = [files]

	totals = [0] * len(params)
	results = []
	for file in files:
		results.append(_get_file_data(file))
	
		
		for i in range(len(params)):
			totals[i] += results[-1][i]

	align = 0
	for total in totals:
		number_length = len(str(total))
		if number_length > align:
			align = number_length

	if not any(params):
		for i, result in enumerate(results):
			print(f'{result[0]:>{align}} {result[1]:>{align}} {result[3]:>{align}}', _get_file_name(files[i]))
		if len(files) > 1:
			print(f'{totals[0]:>{align}} {totals[1]:>{align}} {totals[3]:>{align}}', 'total')
	else:
		for file_i, result in enumerate(results):
			for i, result_value in enumerate(result):
				if params[i]:
					print(f'{result_value:>{align}}', end=' ')
			print(_get_file_name(files[file_i]))
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
		data.custom_name = ''

	if len(files) != 0:
		data = files

	if data is None:
		parser.error('Требуется указать путь до файа или передать текст в стандартный поток ввода')

	wc(data, [args.l, args.w, args.m, args.c])