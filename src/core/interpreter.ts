import {Block, Element} from '../typings/block.ts';
import {Parser} from './parser.ts';
import { existsSync } from 'https://deno.land/std/fs/mod.ts';

export class Interpreter {
  private static _stack: Record<any, any>[] = [];
  private static ast: Block;
  private static cwd: string;
  private static get stack() {
    return this._stack.slice(-1)[0];
  }

  private static addStack() {
    let variables = {};
    for (const stack of this._stack) variables = { ...variables, ...stack };
    this._stack.push(variables);
    return this.stack;
  }

  private static removeStack() {
    this._stack.pop();
    return this.stack;
  }

  private static variableDefinition(node: Block) {
    this.stack[this.processValue(<Element>node[0], 'Identifier')] = this.processValue(<Element>node[1]);
  }

  private static processValue(element: Element, state?: string) {
    if (element.type === 'Word') {
      if (state && state === 'Identifier') return element.value;
      if (this.stack[element.value] !== undefined) return this.stack[element.value];
      return 'none';
    }
    return element.value as string;
  }

  private static process(node: Block | Element): any | void {
    let returned;
    if ('value' in node) return Interpreter.processValue(node);
    for (const child of node) {
      if (Array.isArray(child)) {
        returned = this.process(child);
      } else {
        const [expression, ...args] = node;
        if ('value' in (expression as Element)) {
          const expr = <Element>expression;
          if (expr.value === '{') {
            this.addStack();
            this.process(args);
            this.removeStack();
          }
          if (expr.value === 'let') this.variableDefinition(args);
          else if (expr.value === 'print') {
            console.log(...args.map(this.process));
          };
          return node;
        }
      }
    }
    if (returned) {
      return returned;
    }
    return 'none';
  }
  public static run(source: string, cwd?: string) {
    this.ast = Parser.parse(source);
    this.cwd = cwd || Deno.cwd();
    this.process(this.ast);
    return undefined;
  }
}