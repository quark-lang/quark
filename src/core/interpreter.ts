import {Block, Element} from '../typings/block.ts';
import {Parser} from './parser.ts';

export class Interpreter {
  private static stack: Record<any, any> = {};
  private static ast: Block;
  private static process(node: Block | Element, state?: string): any | void {
    let returned;
    if ('value' in node) {
      if (state && state === 'Identifier') return node.value as string;
      if (node.type === 'Word') {
        if (this.stack[node.value] !== undefined) return this.stack[node.value];
        return node.value as string;
      }
      return node.value as string;
    }
    for (const child of node) {
      if (Array.isArray(child)) {
        returned = this.process(child);
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
            return console.log(...args.map((arg) => this.process(arg)));
            return '';
          } else if (expr.value === 'close') {
            console.log('Stopped due to', ...args.map((arg) => this.process(arg)) + '.');
            Deno.exit();
          } else if (expr.value === 'if') {
            if (this.process(args[0])) return this.process(args[1]);
            return this.process(args[2]);
          } else if (expr.value === '=') {
            return this.process(args[0]) == this.process(args[1]);
          } else if (expr.value === 'fn') {
            const processedArgs = this.process(args[1]);
            const parsedArgs = Array.isArray(processedArgs)
              ? (<Element[]>processedArgs).map((arg: Element) => arg.value)
              : [processedArgs];
            this.stack[this.process(args[0]) as string] = {
              arguments: parsedArgs,
              body: args[2] as Block,
              type: 'function',
              callee: {},
            };
            return this.process(args[1]);
          } else if (expr.value === 'return') {
            return this.process(args[0]);
          } else if (this.process(expr) && (<Record<any, any>>this.process(expr)).type === 'function') {
            const fn: Record<any, any> = this.process(expr) as Record<any, any>;
            for (const index in args) this.stack[fn.arguments[index]] = this.process(args[index]);
            return this.process(fn.body);
          } else if (expr.type === 'String') {
            return this.process(expr);
          }
          return node;
        }
      }
    }
    if (returned) return returned;
    return 'none';
  }
  public static run(source: string) {
    this.ast = Parser.parse(source);
    this.process(this.ast);
    return undefined;
  }
}