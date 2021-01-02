import { Token, Tokens, } from '../typings/token.ts';
import type { Block, } from '../typings/block.ts';
import { Lexer } from './lexer.ts';

export class Parser {
  private static tokens: Token[];
  private static ast: Block = [];
  private static module: boolean;
  private static moduleCounted: number = 0;

  private static findParent(node: Block, root: Block = this.ast): Block | null {
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
      if (['(', '{'].includes(token.value)) {
        ast.push([]);
        if (this.module === true) {
          if (this.moduleCounted > 0) if (token.value === '{') (<Block>ast.slice(-1)[0]).push({ type: 'Node', value: '{' });
          else this.moduleCounted = 1;
        }
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

  public static parse(source: string, module?: boolean) {
    this.tokens = Lexer.tokenize(source);
    this.module = module || false;
    this.ast = [];
    return this.process(0, this.ast);
  }
}