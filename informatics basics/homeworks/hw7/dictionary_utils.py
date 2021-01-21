def _is_word_part(char):
	return char.isalpha() or char.isdigit() or char in ('`', '\'', '’', '-')


def _get_words_from_line(line_index, line):
	word_start = -1
	word = ''

	for i, char in enumerate(line, 1):

		if _is_word_part(char):
			word += char
			if word_start == -1:
				word_start = i

		elif word_start != -1:
			yield word_start, word
			word_start = -1
			word = ''

	if word_start != -1:
		yield word_start, word


def get_dictionary(sources):
	if type(sources) != list:
		sources = [sources]

	dictionary = {}
	for source in sources:
		for line_index, line in enumerate(source.read().split('\n'), 1):
			for start_index, word in _get_words_from_line(line_index, line):
				dictionary[(line_index, start_index)] = word
	return dictionary