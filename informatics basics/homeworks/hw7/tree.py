#!/bin/python3

import argparse
import os
import errno
import sys


def get_tree(directories_only, path='.'):

	def get_tree_recursive(path):
		try:
			fs_objects = os.listdir(path)
		except:
			return {'files': []}

		if directories_only:
			files = []
		else:
			files = list(filter(lambda fs_object: os.path.isfile(os.path.join(path, fs_object)), fs_objects))
		
		directories = list(filter(lambda fs_object: not os.path.isfile(os.path.join(path, fs_object)), fs_objects))
		directory_dict = {'files': files}
		for directory in directories:
			directory_dict['_' + directory] = get_tree_recursive(os.path.join(path, directory))
		return directory_dict


	return (path, get_tree_recursive(path or '.'))


def display_tree(tree, output_path):
	if output_path is not None:
		sys.stdout = open(output_path, 'w+')

	SPACE = ' '
	TREE_SPACE = '   '
	CONNECTOR = '├──'
	CONNECTOR_END = '└──'
	LINE = '│'
	EMPTY_LINE = ' ' * len(LINE)
	BOLD = lambda text: '\033[1m' + text + '\033[0m'

	root = tree[0]
	tree = tree[1]
	print(BOLD(root))

	def display_tree_rec(tree, prefix):
		if len(tree['files']) > 0:
			files = tree['files']
			for file in files[:-1]:
				print(prefix + CONNECTOR + SPACE + file)	

			print(prefix  + (CONNECTOR if len(tree) > 1 else CONNECTOR_END) + SPACE + files[-1])

		if len(tree) > 1:
			directories = list(tree.items())[1:]

			for directory, directory_tree in directories[:-1]:
				directory = directory[1:] #  убираем нижнее подчёркивание '_', добавленное в get_tree()
				print(prefix + CONNECTOR + SPACE + BOLD(directory))
				display_tree_rec(directory_tree, prefix + LINE + TREE_SPACE)

			print(prefix + CONNECTOR_END + SPACE + BOLD( "_".join(directories[-1][0].split('_')[1:])))
			display_tree_rec(directories[-1][1], prefix + TREE_SPACE + EMPTY_LINE)


	display_tree_rec(tree, '')


def process_path_argument(parser, argument):
	if os.path.exists(argument):
		if not os.path.isfile(argument):
			parser.error(f'Требуется указать путь до файла')
		elif not os.access(argument, os.W_OK):
			parser.error(f'Нет доступа на запись в файл {argument}')
		else:
			return argument
	else:
		try:
			os.makedirs(os.path.dirname(argument))
		except OSError as error:
			if error.errno != errno.EEXIST:
				parser.error(f'Не удается создать файл {argument}. Ошибка: {error}')
		
		return argument


parser = argparse.ArgumentParser(add_help=False)
parser.add_argument('-d', dest='directories_only', action='store_true')
parser.add_argument('-o', dest='output_path', type=lambda path: process_path_argument(parser, path))
args, unknowns = parser.parse_known_args()

if __name__ == '__main__':
	
	for path in unknowns:
		if not os.path.exists(path) or not os.path.isdir(path) or not os.access(path, os.R_OK):
			sys.stderr.write(f'{__file__}: не удалось открыть директорию {path}\n')
		else:
			display_tree(get_tree(args.directories_only, path), args.output_path)
	
	if len(unknowns) == 0:
		display_tree(get_tree(args.directories_only), args.output_path)