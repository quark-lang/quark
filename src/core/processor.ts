import {
  Token,
  Tokens,
} from '../typings/token.ts';
import { Block } from '../typings/block.ts';
import { Lexer } from './lexer.ts';

export class Processor {
  private static tokens: Token[];
  private static ast: Block = [];
  private static parents: number[] = [];

  private static goTo(it: number = this.parents.length, ast: Block = this.ast): Block {
    if (it === 0) return ast;
    this.parents.splice(0, 1);
    return this.goTo(it - 1, ast[it - 1] as Block);
  }

  private static parse(index: number, ast: Block): Block {
    const token: Token = this.tokens[index];
    if (!token) return this.ast;
    if (token.token === Tokens.Node) {
      if (['(', '{'].includes(token.value)) {
        this.parents.push(ast.length + 1);
        ast.push([]);
        return this.parse(index + 1, ast.slice(-1)[0] as Block);
      } else {
        return this.parse(index + 1, this.goTo(1));
      }
    } else if ([Tokens.String, Tokens.Word].includes(token.token)) {
      ast.push(token.value);
    }
    return this.parse(index + 1, ast);
  }

  public static process(source: string) {
    this.tokens = Lexer.tokenize(source);
    return this.parse(0, this.ast);
  }
}
