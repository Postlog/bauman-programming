function memoize(func) {
	let memo = {}
	return function(...args) {
		if(args in memo) return memo[args]
		else return memo[args] = func(...args), memo[args]
	}
}

module.exports = memoize