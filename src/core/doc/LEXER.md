# Quark lang's lexer
Quark lexer let you transform some code to a simple list of tokens which you'll be to easily parse.

### Formatting content
First of all, Quark need to make code as clearly as possible. So it will format it to a single line of code trimmed, ordered and sorted.

```ts
this.code = source
  .split(/\r?\n/g)
  .map((line: string) => line.trim())
  .join('');
```

- Source going to be split into an array which contains every line of your code.
- Then formatter will remove empty lines from array
- Formatter finally return the joined array into a string.

### Lexing formatted code
When formatting end, we have to lexer the code in order to return a token's array.

#### Variable definitions
```ts
let state: string = '';
const container: Token[] = [];
const tmp: string[] = [];
```

- State variable hold the current temporary code state. Now it can be only **empty** or **String**.
- Container hold the final token list which will be returned.
- Temporary variable (or `tmp`) hold the lexed chars till pushing it to container.

#### Node lexing
In a first time, lexer will be checking if current char is a **Node char**. Node chars are:
- `(` and `)` are Lisp syntax base. They specify that a block definition begins
-  `{` and `}` mean node declaration. A Node is a group of blocks.

```ts
if (['(', ')', '{', '}'].includes(char)) {
  if (tmp.length > 0) {
    state = '';
    container.push({ token: Tokens.Word, value: tmp.join('').trim() });
    tmp.splice(0, tmp.length);
  }
  container.push({ token: Tokens.Node, value: char as Node, });
} 
```

With this code, lexing is firstly checking if it's a Node char or not. Then it's checking if temporary array if empty, it's useful in theses cases:

```lisp
(let username "Thomas")
(print "Hello" username)
```

It let avoiding code's forgetfulness with pushing temporary code to container.
After checking if temporary code isn't empty, char will be pushed to container as Node.

#### String lexing
In order to lex strings, lexer will be checking if char is a double quote.

```ts
if (char === '"') {
  tmp.push(char);
  if (state === Tokens.String) {
    state = '';
    container.push({ token: Tokens.String, value: tmp.join('').trim() });
    tmp.splice(0, tmp.length);
  } else {
    state = Tokens.String;
  }
} 
```

Afterwards, it pushes char to temporary code and checking after if string state has already been set.
- If it has, lexer empty the state, push temporary code to container and clear temporary code.
- If it hasn't, lexer set the state to `String`.

#### Space lexing
Space checking may be a little tricky, its lexing depends on current code state.
```ts
if (char === ' ' && tmp.length > 0) {
  if (state === Tokens.String) {
    tmp.push(char);
  } else {
    state = '';
    container.push({ token: Tokens.Word, value: tmp.join('').trim() });
    tmp.splice(0, tmp.length);
  }
}
```

So lexer check if char is a space **and** if temporary code isn't empty because of these case:
```lisp
(print "test"   )
(print     "Hello")
```

If lexer wasn't checking that, it will return an array with many empty tokens.
After, lexer check state:
- If state is string then char is simply added to temporary code.
- If it isn't, lexer is doing the same thing here as he did in other lexing, it's resetting the state, pushing temporary code to container and clearing temporary code.

#### Other char lexing
Lexer simply push char to temporary code.

#### Final step
Before returning container, lexer is checking for empty tokens and remove it.
