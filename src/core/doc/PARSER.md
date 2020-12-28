# Quark lang's parser
Quark parser is one of the most important step in the language. It transforms token list into a multi-depth array which can be easily interpreted or compiled. Moreover, it's a lightweight and simple way to parse code.

### Block definition
A block is an element in Quark which can contain strings or blocks. So it's a recursive type.
```ts
export type Block = (string | Block)[];
```

## Parsing

### Finding parent method
Finding parent method let parser find corresponding parent of node with recursive way.
```ts
private static findParent(node: Block, root: Block = this.ast): Block | null {
  let found: Block | null = null;
  for (const child of root) {
    if (child === node) {
      return root;
    } else {
      if (typeof child === 'string') continue;
      found = this.findParent(node, child);
      if (found !== null) return found;
    }
  }
  return null;
}
```

So the step that method does are:
 - Defining a variable which will contains future self-call return value
 - Looping through root elements:
   - If child element is the same as first argument, returning child's parent.
   - Else, checking if type of child is string and then skipping returning, else checking if found is not null and then returning found.
    
### Parse method
It is called `processor` due to existing public `parse` method and due to it processing meaning too. It returns an `Block` which can be easily interpreted or parsed.

#### Checking for undefined token
```ts
const token: Token = this.tokens[index];
if (!token) return this.ast;
```

In the first time, we're defining token variable to avoid redundancy code and then checking it token existing or not and returning if not to avoid errors.

#### Node parsing
```ts
if (token.token === Tokens.Node) {
  if (['(', '{'].includes(token.value)) {
    ast.push([]);
    return this.process(index + 1, ast.slice(-1)[0] as Block);
  } else {
    return this.process(index + 1, this.findParent(ast, this.ast) as Block);
  }
}
```
Node checking is quite simple: In a first time, we're checking if it's a node opener or not to determine which action to execute:
 - If it is, we push a new node to ast and returning self method call of the last pushed element, so the new node.
 - If it isn't, we return self method call of parent element

#### Other tokens parsing

```ts
if ([Tokens.String, Tokens.Word].includes(token.token)) {
  ast.push(token.value);
}
return this.process(index + 1, ast);
```

We simply check if current token is a String or a Word and then pushing token's value to ast.
Finally we return self method call of current ast branch.

[Back to table of content](../README.md)