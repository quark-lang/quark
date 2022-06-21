const fromList = (ls = []) => {
	if (ls.length === 0) return { type: 'Nil' };
	const [x, ...xs] = ls;
	return { type: 'Cons', v0: x, v1: fromList(xs) };
}