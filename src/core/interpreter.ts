import { Block } from '../typings/block.ts';
import { Parser } from './parser.ts';

export class Interpreter {
  private static ast: Block;
  public static run(source: string) {
    this.ast = Parser.parse(source);
    console.log(this.ast);
    return this.ast;
  }
}