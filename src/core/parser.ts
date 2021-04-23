import { Token, Tokens, } from '../typings/token';
import type { Block, } from '../typings/block';
import { Lexer } from './lexer';

export class Parser {
  private static tokens: Token[];
  private static ast: Block = [];

  public static findParent(node: Block, root: Block = this.ast): Block | null {
    let found: Block | null = null;
    for (const child of root) {
      if (child === node) return root;

      if ('type' in child && 'value' in child) continue;
      found = this.findParent(node, child);
      if (found !== null) return found;
    }
    return null;
  }

  private static process(index: number, ast: Block): Block {
    const token: Token = this.tokens[index];
    if (!token) return this.ast;
    if (token.token === Tokens.Node) {
      if (token.value === '(') {
        ast.push([]);
        return this.process(index + 1, ast.slice(-1)[0] as Block);
      } else if (token.value === '[') {
        ast.push([{ 
          type: 'Word',
          value: 'list', }]);
        return this.process(index + 1, ast.slice(-1)[0] as Block);
      } else if (token.value === '{') {
        ast.push([{ 
          type: 'Word',
          value: 'begin', }]);
        return this.process(index + 1, ast.slice(-1)[0] as Block);
      }
      return this.process(index + 1, this.findParent(ast, this.ast) as Block);
    }
    if (token.value.startsWith('"')) {
      ast.push({
        type: 'String',
        value: token.value.slice(1, token.value.length - 1),
      });
    } else if (!isNaN(Number(token.value))) {
      ast.push({
        type: 'Number',
        value: Number(token.value),
      });
    } else {
      ast.push({
        type: 'Word',
        value: token.value,
      });
    }
    return this.process(index + 1, ast);
  }

  public static parse(source: string, file: string) {
    this.tokens = Lexer.tokenize(source, file);
    this.ast = [];
    return this.process(0, this.ast);
  }
}