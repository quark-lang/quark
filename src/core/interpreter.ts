import { Block, Element } from '../typings/block.ts';
import { Parser } from './parser.ts';

export class Interpreter {
  private static stack: Record<any, any> = {};
  private static ast: Block;
  private static process(node: Block | Element, state?: string): Block | Element | string | Record<any, any> | boolean {
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
          if (['+', '-', '/', '*'].includes(expr.value as string)) {
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
            switch (expr.value) {
              case '+': return parsedArgs.reduce((acc, cur) => acc + cur);
              case '-': return parsedArgs.reduce((acc, cur) => acc - cur);
              case '*': return parsedArgs.reduce((acc, cur) => acc * cur);
              case '/': return parsedArgs.reduce((acc, cur) => acc / cur);
            }
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
          } else if (expr.value === 'if') {
            if (this.process(args[0])) {
              return this.process(args[1]);
            } else return this.process(args[2]);
          } else if (expr.value === '=') {
            return this.process(args[0]) == this.process(args[1]);
          } else if (expr.value === 'func') {
            this.stack[(<Element>args[0]).value] = {
              arguments: (<Element[]>this.process(args[1])).map((arg: Element) => arg.value),
              body: args[2] as Block,
            };
            return args[1];
          } else {
            const fn = this.stack[(<Element>expr).value];
            if (fn) {
              for (const arg in args) this.stack[fn.arguments[arg]] = this.process(args[arg]);
              this.process(fn.body);
              for (const arg in args) delete this.stack[fn.arguments[arg]];
              return expr;
            }
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