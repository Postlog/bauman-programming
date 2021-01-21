import re


def is_word_part(char):
	return char.isalpha() or char.isdigit() or char in ('`', '\'', '’', '-')


def tokenize(sources):
	if type(sources) != type([]):
		sources = [sources]
	dictionary = {}
	for source in sources:
		for line_index, line in enumerate(source.read().split('\n'), 1):
			word_start = -1
			word = ''
			for i, char in enumerate(line, 1):
				if is_word_part(char):
					word += char
					if word_start == -1:
						word_start = i
					continue
				else:
					if word_start != -1:
						dictionary[(line_index, word_start)] =  word
						word = ''
						word_start = -1

			if word_start != -1:
				dictionary[(line_index, word_start)] =  word
	return dictionary