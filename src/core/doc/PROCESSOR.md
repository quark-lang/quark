# Quark lang's processor
Quark processor is a part of the parsing, it transforms tokens container into an ordered multi-depth array which can be afterwards easily parsed into AST.

> Why calling it `procesor` ?

It's called `processor` because it's the step that comes before parsing. Tokens are processed in order to be easily parsed.

### Parent visitor
Important function in parsing is the parent visitor which let you coming back to parent from a current block. It's necessary because of the recursion processing.
```ts
private static goTo(it: number = this.parents.length, ast: Block = this.ast): Block {
  if (it === 0) return ast;
  this.parents.splice(0, 1);
  return this.goTo(it - 1, ast[it - 1] as Block);
}
```

So in a first time, `goTo` function takes two parameters:
- An `it` parameter which is simply the number of parents we have to coming back.
- A `ast` parameter which contains the current ast: the current block came back.

After calling the function, `goTo` is checking if iteration's number reached.
 - If it has, function return ast.
 - Otherwise, function is removing the first parent's index of parents container and returning then a call of `goTo` with a decrement of iteration number and the following ast block.

### Processing
In Quark, processing is a recursive task which returns an array with either string or array with same attributes, so simply a recursive array. 
#### Undefined token
```ts
const token: Token = this.tokens[index];
if (!token) return this.ast;
```
Token variable is defined to make code simpler and clearer. Processor is obviously checking if token is defined or not, if tokens list reach the end.
In case of reaching end, function return complete ast.

#### Node processing
Node processing way, explanation and functioning may be tricky. Generally speaking, parser check if it's an opener token:

- If it is, parser pushes the current index to parent list in order to stock the future parent position. It pushes then child array to ast and return a call of itself with arguments:
  - Index incrementation
  - Last element of current ast, in other words, the child.
  ```ts
  if (['(', '{'].includes(token.value)) {
    this.parents.push(ast.length + 1);
    ast.push([]);
    return this.parse(index + 1, ast.slice(-1)[0] as Block);
  }
  ```
- If it isn't, parser return a call of itself with the parent node.
  ```ts
  else {
    return this.parse(index + 1, this.goTo(1));
  }
  ```
#### String and word processing
If token is string or word, parser simply pushes current token value to ast as a string:

```ts
if ([Tokens.String, Tokens.Word].includes(token.token)) {
  ast.push(token.value);
}
```

Then it returns a call of itself to the next token.

```ts
return this.parse(index + 1, ast);
```

[Back to table of content](../README.md)