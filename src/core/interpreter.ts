import { Block, Element } from '../typings/block.ts';
import { Parser } from './parser.ts';
import { existsSync } from 'https://deno.land/std/fs/mod.ts';

export class Interpreter {
  private static _stack: Record<any, any>[] = [];
  private static ast: Block;
  private static cwd: string;
  private static get stack() {
    if (this._stack.length === 0) this.pushStackFrame();
    return this._stack.slice(-1)[0];
  }

  private static pushStackFrame() {
    let variables = {};
    for (const stack of this._stack) variables = { ...variables, ...stack };
    this._stack.push(variables);
    return this.stack;
  }

  private static popStackFrame() {
    this._stack.pop();
    return this.stack;
  }

  private static variableDefinition(node: Block) {
    this.stack[this.process(<Element>node[0], 'Identifier')] = this.process(<Element>node[1]);
  }

  private static processValue(element: Element, state?: string) {
    if (element.value === 'stack') return this.stack;
    if (element.type === 'Word') {
      if (state && state === 'Identifier') return element.value;
      if (this.stack[element.value] !== undefined) return this.stack[element.value];
      return 'none';
    }
    return element.value as string;
  }

  private static processArithmetic(operation: string, args: Block) {
    switch (operation) {
      case '+': return args.reduce((acc: any, cur: any) => this.process(acc) + this.process(cur));
    }
  }

  private static functionDefinition(node: Block) {
    const [args, body] = node;
    const functionArguments: string[] = [];
    // @ts-ignore
    for (const arg of args) {
      const argumentName: string = this.processValue(<Element>arg, 'Identifier');
      functionArguments.push(argumentName);
    }
    return {
      args: functionArguments,
      body,
      type: 'Function'
    };
  }

  private static callFunction(node: Block, functionName: string) {
    const values = [];
    for (const arg of node) values.push(this.process(arg));
    this.pushStackFrame();
    const fn = this.stack[functionName]
    for (const index in fn.args) this.stack[fn.args[index]] = values[Number(index)];
    for (const el of fn.body) {
      const res = this.process(el);
      if (res[1] && res[1] === true) return res[0];
    }
    const lastStatement = fn.body.slice(-1)[0];
    if (lastStatement.length > 1 && lastStatement[0].value !== 'return') return 'none';
    if (lastStatement) return this.process(lastStatement);
    this.popStackFrame();
    return 'none';
  }

  private static processReturn(node: Block) {
    const value = this.process(node[0]);
    this.popStackFrame();
    return [value, true];
  }

  private static processCondition(node: Block) {
    if (this.process(node[0])) return this.process(node[1]);
    if (node[2]) return this.process(node[2]);
    return [undefined, false]
  }

  private static processEqualities(operation: string, lhs: Block | Element, rhs: Block | Element) {
    switch (operation) {
      case '=': return this.process(lhs) == this.process(rhs);
    }
  }

  private static processImport(node: Block) {
    const path = this.cwd + '/' + (<Element>node[0]).value;
    if (!existsSync(path)) throw `File ${path} does not exists!`;
    const array = Deno.readFileSync(path);
    const content: string = new TextDecoder('utf-8').decode(array);

    const ast = Parser.parse(content, true);
    this.process(ast);
  }

  private static process(node: Block | Element, state?: string): any | void {
    let returned;
    if (typeof node === 'string') return node;
    if ('value' in node) return Interpreter.processValue(node, state);
    for (const child of node) {
      if (Array.isArray(child)) {
        returned = this.process(child);
      } else {
        const [expression, ...args] = node;
        if ('value' in (expression as Element)) {
          const expr = <Element>expression;
          if (expr.value === '{') {
            this.pushStackFrame();
            this.process(args);
            this.popStackFrame();
          }
          else if (expr.value === 'import') Interpreter.processImport(args);
          else if (expr.value === 'let') this.variableDefinition(args);
          else if (expr.value === 'print') console.log(...args.map(arg => this.process(arg)));
          else if (expr.value === '+') return Interpreter.processArithmetic(expr.value, args);
          else if (expr.value === 'fn') return Interpreter.functionDefinition(args);
          else if (expr.value === 'return') return Interpreter.processReturn(args);
          else if (expr.value === '=') return Interpreter.processEqualities(expr.value, args[0], args[1]);
          else if (expr.value === 'if') return Interpreter.processCondition(args);
          else if (Interpreter.stack[expr.value].type === 'Function') return Interpreter.callFunction(args, expr.value as string);
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