import { Processor } from './processor.ts';
import { Block } from '../typings/block.ts';

type Node = Record<string , string | Node[]>;

export class Parser {

  public static parse(source: string) {
    const ast: Block = Processor.process(source);
    return this.process(0, ast, this.ast);
  }
}