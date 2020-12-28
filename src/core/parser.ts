import {
  Token,
  Tokens,
} from '../typings/token.ts';
import { Block } from '../typings/block.ts';
import { Lexer } from './lexer.ts';

export class Parser {
  private static tokens: Token[];
  private static ast: Block = [];
  private static parents: number[] = [];

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

  private static process(index: number, ast: Block): Block {
    const token: Token = this.tokens[index];
    if (!token) return this.ast;
    if (token.token === Tokens.Node) {
      if (['(', '{'].includes(token.value)) {
        ast.push([]);
        return this.process(index + 1, ast.slice(-1)[0] as Block);
      } else {
        return this.process(index + 1, this.findParent(ast, this.ast) as Block);
      }
    } else if ([Tokens.String, Tokens.Word].includes(token.token)) {
      ast.push(token.value);
    }
    return this.process(index + 1, ast);
  }

  public static parse(source: string) {
    this.tokens = Lexer.tokenize(source);
    return this.process(0, this.ast);
  }
}