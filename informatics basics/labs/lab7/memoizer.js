function memoize(func) {
	let memo = {}

	return function() {
		let stringArguments = Array.from(arguments).toString()
		
		if(stringArguments in memo) return memo[stringArguments]
		else return memo[stringArguments] = func(...arguments), memo[stringArguments]
	}
}

module.exports = memoize