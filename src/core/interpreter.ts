import type { Block, Element } from '../typings/block.ts';
import { Parser } from './parser.ts';
import { existsSync } from 'https://deno.land/std/fs/mod.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';
import { File } from '../utils/file.ts';

export class Interpreter {
  private static _stack: Record<any, any>[] = [];
  private static ast: Block;
  static cwd: string;
  static get stack() {
    if (this._stack.length === 0) this.pushStackFrame();
    return this._stack.slice(-1)[0];
  }

  private static pushStackFrame() {
    this._stack.push({});
    return this.stack;
  }

  private static findValueInFrame(value: string): Record<any, any>  | undefined {
    if (this._stack.length === 0) {
      this.pushStackFrame();
      return undefined;
    }
    for (const index in this._stack.slice().reverse()) {
      if (this._stack[index][value] !== undefined) {
        return {
          index: index,
          value,
        };
      }
    }
    return undefined;
  }

  private static popStackFrame() {
    this._stack.pop();
  }

  private static async variableDefinition(node: Block) {
    const variable = await this.process(<Element>node[0], 'Identifier');
    const value = await this.process(<Element>node[1]);
    this.stack[variable] = value;
  }

  private static async variableModification(node: Block) {
    const variable = await this.process(<Element>node[0], 'Identifier');
    const value = await this.process(<Element>node[1]);
    this.stack[variable] = value;

    const stackElement = this.findValueInFrame(variable);
    if (stackElement) this._stack[stackElement.index][stackElement.value] = value;
    else await this.variableDefinition(node);
  }

  private static processValue(element: Element, state?: string) {
    if (element.value === 'none') return undefined;
    if (element.value === 'stack') return this.stack;
    if (element.type === 'Word') {
      if (state && state === 'Identifier') return element.value;
      const stackElement = this.findValueInFrame(element.value as string);
      const value = stackElement ? this._stack[stackElement.index][stackElement.value] : undefined;
      if (value !== undefined) return stackElement;
      return 'none';
    }
    return element.value as string;
  }

  private static async processArithmetic(operation: string, args: Block): Promise<any> {
    // @ts-ignore
    let result: any = await this.process(args[0]);
    for (const arg of args.slice(1)) {
      if (operation === '+') {
        result += await this.process(arg);
      } else if (operation === '-') {
        result -= await this.process(arg);
      } else if (operation === '*') {
        result *= await this.process(arg);
      } else if (operation === '/') {
        result /= await this.process(arg);
      }
    }
    return result;
  }

  private static async processList(args: Block, state?: string): Promise<any> {
    const array = [];
    if (state === 'Identifier') return {
      arg: await this.process(args[0], 'Identifier'),
      variadic: true,
    };
    for (const arg of args) array.push(await this.process(arg));
    return array;
  }

  private static async functionDefinition(node: Block) {
    const [args, body] = node;
    const functionArguments: any[] = [];
    // @ts-ignore
    if (args.length > 0) for (const arg of args) {
      const argumentName: string = await this.process(<Element>arg, 'Identifier');
      if (typeof argumentName === 'string') functionArguments.push({
        arg: argumentName,
        variadic: false,
      });
      else functionArguments.push(argumentName);
    }
    return {
      args: functionArguments || [],
      body,
      type: 'Function',
      js: false,
    };
  }

  static async callFunction(args: any[], functionName: any) {
    const callArguments = [];
    for (const arg of args) {
      const processed = await this.process(arg);
      const value = this.isObject(processed) ? this._stack[processed.index][processed.value] : processed;
      callArguments.push(value);
    }
    const foundFunc = this.findValueInFrame(functionName);
    const func = foundFunc ? this._stack[foundFunc.index][foundFunc.value] : undefined;
    if (func === undefined) throw `Function ${functionName} does not exists!`;
    this.pushStackFrame();
    for (const index in func.args) {
      const arg = func.args[index];
      if (arg.variadic === false) this.stack[arg.arg] = callArguments[Number(index)];
    }
    if ((<Block>func.body).every((child) => Array.isArray(child))) {
      for (const instruction of func.body) {
        const res = await this.process(instruction);
      }
    } else return await this.process(func.body);
    this.popStackFrame();
    return 'none';
  }

  private static async processSpread(node: Block) {
    return {
      variadic: true,
      value: await this.process(node[0]),
    }
  }

  private static async processReturn(node: Block) {
    const value = await this.process(node[0]);
    return [value, true];
  }

  private static async processCondition(node: Block) {
    if (await this.process(node[0])) return await this.process(node[1]);
    else if (node[2]) return await this.process(node[2]);
    else return [undefined, false];
  }

  private static async processEqualities(operation: string, lhs: Block | Element, rhs: Block | Element) {
    switch (operation) {
      case '=': return await this.process(lhs) == await this.process(rhs);
      case '!=': return await this.process(lhs) != await this.process(rhs);
      case '<': return await this.process(lhs) < await this.process(rhs);
      case '>': return await this.process(lhs) > await this.process(rhs);
      case '<=': return await this.process(lhs) <= await this.process(rhs);
      case '>=': return await this.process(lhs) >= await this.process(rhs);
      case 'and': return await this.process(lhs) && await this.process(rhs);
      case 'or': return await this.process(lhs) || await this.process(rhs);
    }
  }

  public static parentDir(src: string, it: number = 1): string {
    for (let i = 0; i < it; i++) src = path.dirname(src);
    return src;
  }

  private static async processImport(node: Block) {
    let src: string | URL = ((<Element>node[0]).value as string).replace(/:/g, '/');
    // Coming 3 folders back due to Interpreter path
    const stdPath: string = path.join(this.parentDir(path.fromFileUrl(import.meta.url), 3), 'std');
    // Checking if file exists and setting correct path
    let finalPath: string | undefined = undefined;
    if (existsSync(path.join(stdPath, src))) finalPath = path.join(stdPath, src);
    else if (existsSync(path.join(this.cwd, src))) finalPath = path.join(this.cwd, src);
    if (!finalPath) throw 'Request module file ' + src + ' not found!';

    // Getting file extension for choosing which parsing to use
    const ext: string = path.extname(finalPath);
    if (ext === '.qrk') {
      const content: string = await File.read(finalPath);
      // Processing with module directory as cwd
      await this.process(Parser.parse(content, true), undefined, this.parentDir(finalPath))
    } else {
      await import(finalPath);
    }
  }

  private static isObject(variable: any) {
    return !Array.isArray(variable) && typeof variable === 'object';
  }

  private static async processWhile(node: Block) {
    const [condition, body] = node;
    while (await this.process(condition)) {
      if ((<Block>body).every((child) => Array.isArray(child))) {
        for (const el of body) {
          const res = await this.process(el);
          if (res && res[1] && res[1] === true) return res;
        }
      } else {
        const res = await this.process(body);
        if (res && res[1] && res[1] === true) return res;
      }
    }
  }

  private static async processListIndex(node: Block, state?: string) {
    const [array, index] = node;
    if (state && state === 'Identifier') {
      return {
        variable: await this.process(array, 'Identifier'),
        index: await this.process(index),
      };
    }
    return (await this.process(array))[await this.process(index)];
  }

  private static async process(node: Block | Element, state?: string, cwd: string = this.cwd): Promise<any> {
    let returned;
    if (typeof node === 'string') return node;
    if ('value' in node) {
      return Interpreter.processValue(node, state);
    }
    for (const child of node) {
      if (Array.isArray(child)) {
        returned = await this.process(child, undefined, cwd);
      } else {
        const [expression, ...args] = node;
        if ('value' in (expression as Element)) {
          const expr = <Element>expression;
          if (expr.value === '{') {
            this.pushStackFrame();
            await this.process(args, undefined, cwd);
            this.popStackFrame();
          }
          else if (expr.value === 'import') return await Interpreter.processImport(args);
          else if (expr.value === 'print') {
            const values = [];
            for (const arg of args) {
              const processed = await this.process(arg);
              const value = processed ? this.isObject(processed) ? this._stack[processed.index][processed.value] : processed : undefined;
              values.push(value);
            }
            return console.log(...values);
          }
          else if (expr.value === 'let') return await this.variableDefinition(args);
          else if (expr.value === 'set') return await this.variableModification(args);
          else if (['+', '-', '/', '*'].includes(expr.value as string)) return await Interpreter.processArithmetic(expr.value as string, args);
          else if (expr.value === 'fn') return await Interpreter.functionDefinition(args);
          else if (expr.value === 'return') return await Interpreter.processReturn(args);
          else if (['=', '!=', '<', '>', '<=', '>=', 'and', 'or'].includes(expr.value as string)) return await Interpreter.processEqualities(expr.value as string, args[0], args[1]);
          else if (expr.value === 'if') return await Interpreter.processCondition(args);
          else if (expr.value === 'spread') return await Interpreter.processSpread(args);
          else if (expr.value === 'list') return await Interpreter.processList(args, state);
          else if (expr.value === 'while') return await Interpreter.processWhile(args);
          else if (expr.value === 'index') return await Interpreter.processListIndex(args, state);
          const stackElement = this.findValueInFrame(expr.value as string);
          const value = stackElement !== undefined ? this._stack[stackElement.index][stackElement.value] : undefined;
          if (value && value.type === 'Function') return await Interpreter.callFunction(args, expr.value as string);
          throw 'test';
        }
      }
    }
    if (returned) {
      return returned;
    }
    return 'none';
  }
  public static async run(source: string, src: string) {
    this.ast = Parser.parse(source);
    this.cwd = src;
    await this.process(this.ast);
    return undefined;
  }
}