#!/bin/python3

import argparse
import sys


def get_stream_contents_info(stream):
	contents = stream.read()
	return [contents.count('\n'),  	#  lines count
			len(contents.split()), 	#  words count
			len(contents), 			#  chars count 
			len(contents.encode('utf-8'))]     	#  bytes count


def get_stream_name(stream):
	if hasattr(stream, 'custom_name'):
		return stream.custom_name
	else:
		return stream.name


def display(elements, align, postfix):
	for element in elements:
		print(f'{element:>{align}}', end=' ')
	print(postfix)


def get_required_params_indexes(params):
	indexes = []
	for i, param in enumerate(params):
		if param:
			indexes.append(i)
	return indexes


def wc(streams, params):
	if not any(params):
		params[0] = params[1] = params[3] = True

	required_params_indexes = get_required_params_indexes(params)

	totals = [0] * len(required_params_indexes)
	results = []

	for stream in streams:
		info = get_stream_contents_info(stream)
		required_info = [info[i] for i in required_params_indexes] 
		
		results.append(required_info)
		
		for i in range(len(totals)):
			totals[i] += required_info[i]

	align = max([len(str(total)) for total in totals])

	for i, result in enumerate(results):
		display(result, align, get_stream_name(streams[i]))

	if len(streams) > 1:
		display(totals, align, 'total')


def parse_unknowns(parser, unknowns):
	streams = []
	for unknown in unknowns:
		try:
			streams.append(open(unknown, 'r'))
		except:
			sys.stderr.write(f'{__file__}: не удается открыть указанный файл {unknown}\n')
	return streams


parser = argparse.ArgumentParser(add_help=False)
parser.add_argument('-c', action='store_true')
parser.add_argument('-m', action='store_true')
parser.add_argument('-w', action='store_true')
parser.add_argument('-l', action='store_true')
args, unknowns = parser.parse_known_args()


if __name__ == '__main__':
	streams = parse_unknowns(parser, unknowns)

	if not sys.stdin.isatty() and len(streams) == 0:
		streams.append(sys.stdin)
		streams[0].custom_name = '' # добавляем кастомное пустое имя, чтобы не отображать его (<stdin>) при выводе

	if len(streams) == 0:
		parser.error('Требуется указать путь до файа или передать текст в стандартный поток ввода')

	wc(streams, [args.l, args.w, args.m, args.c])