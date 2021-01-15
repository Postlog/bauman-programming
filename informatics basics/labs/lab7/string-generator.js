function get_random_int(start, end) {
	return Math.floor(Math.random() * (end - start + 1)) + start;
}

function get_random_strings(count, length) {
	if(!Number.isInteger(count) || !Number.isInteger(length))
		throw new TypeError('invalid argument types')

	if(count <= 0 || length <= 0)
		throw new Error('invalid argument values')

	let first_charcode = 33,
		last_charcode = 126;

	let strings = Array(count).fill('')

	for(let i = 0; i < count; i++)
		for(let _ = 0; _ < length; _++)
			strings[i] += String.fromCharCode(get_random_int(first_charcode, last_charcode));
	return strings
}

module.exports = get_random_strings