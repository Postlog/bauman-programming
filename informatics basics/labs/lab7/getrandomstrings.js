function getRandomInt(min, max) {
	return Math.floor(Math.random() * (max - min + 1)) + min;
}

function getRandomStrings(count, length) {
	if (!Number.isInteger(count) || !Number.isInteger(length))
		throw new TypeError('invalid argument types')

	if (count <= 0 || length <= 0)
		throw new Error('invalid argument values')

	let firstCharCode = 33,
		lastCharCode = 126;

	let strings = Array(count).fill('')

	strings.forEach((_, index) => {
		for (let _ = 0; _ < length; _++)
			strings[index] += String.fromCharCode(getRandomInt(firstCharCode, lastCharCode))
	})
	return strings
}

module.exports = getRandomStrings