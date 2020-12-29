import { Block, Element } from '../typings/block.ts';
import { Parser } from './parser.ts';

export class Interpreter {
  private static stack: Record<any, any> = {};
  private static ast: Block;
  private static process(node: Block | Element, state?: string): Block | Element | string | Record<any, any> {
    if ('value' in node) {
      if (state && state === 'Identifier') return node.value as string;
      if (node.type === 'Word') {
        if (node.value === 'stack') return this.stack;
        if (this.stack[node.value]) return this.stack[node.value];
        return node.value as string;
      }
      return node.value as string;
    }
    for (const child of node) {
      if (Array.isArray(child)) {
        this.process(child);
      } else {
        const [expression, ...args] = node;
        if ('value' in (expression as Element)) {
          const expr = <Element>expression;
          if (expr.value === '+') {
            if (state === 'Identifier') {
              (<Element[]>args).map((arg) => arg.type = 'Word');
            }
            const parsedArgs = args.map((acc) => {
              if ('value' in acc && acc.type === 'Number') {
                return Number(this.process(acc)) as any;
              }
              return this.process(acc) as any;
            });
            if (parsedArgs.length === 0) return '';
            return parsedArgs.reduce((acc, cur) => acc + cur);
          } else if (expr.value === 'let') {
            const id: string = this.process(args[0] as Block, 'Identifier') as string;
            this.stack[id] = this.process(args[1] as Block);
            return this.stack[id];
          } else if (expr.value === 'print') {
            console.log(...args.map((arg) => this.process(arg)));
            return node;
          } else if (expr.value === 'close') {
            console.log('Stopped due to', ...args.map((arg) => this.process(arg)) + '.');
            Deno.exit();
          }
        }

      }
    }
    return node;
  }
  public static run(source: string) {
    this.ast = Parser.parse(source);
    this.process(this.ast);
    return undefined;
  }
}