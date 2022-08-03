const toString = str => {
  if (str.type === 'Cons') {
    const x = str.v0;
    const xs = str.v1;
    return `${x}${toString(xs)}`;
  }
  if (str.type === 'Nil') return '';
}

const read = str => Number(toString(str));