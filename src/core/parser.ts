import { Processor } from './processor.ts';
import { Block } from '../typings/block.ts';

type Node = Record<string , string | Node[]>;

export class Parser {
  private static ast: Node = {
    type: 'Program',
    body: [],
  }
  private static process(index: number, ast: Block, newAst: Node) {
    return this.ast;
  }

  public static parse(source: string) {
    const ast: Block = Processor.process(source);
    return this.process(0, ast, this.ast);
  }
}