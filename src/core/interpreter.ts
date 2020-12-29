import { Block } from '../typings/block.ts';
import { Parser } from './parser.ts';

export class Interpreter {
  private static ast: Block;
  private static process(node: Block, state?: string) {
    for (const child of node) {
      if (Array.isArray(child)) {
        this.process(child);
      } else {
        const [expression, ...args] = node;
        if (expression === 'let') {
          console.log('Identifier:', this.process(args[0] as Block, 'Identifier'));
          console.log('Value:', this.process(args[1] as Block));
        } else if (expression === '+') {
          if (state && state === 'Identifier') {
            args[0] = args[0].slice(1, args[0].length - 1);
            args[1] = args[1].slice(1, args[1].length - 1);
          }
          return args[0] + <string>args[1];
        }
      }
    }
  }
  public static run(source: string) {
    this.ast = Parser.parse(source);
    this.process(this.ast);
    return this.ast;
  }
}