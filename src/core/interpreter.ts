import type { Block, Element } from '../typings/block.ts';
import { Parser } from './parser.ts';
import { existsSync } from 'https://deno.land/std/fs/mod.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';
import { File } from '../utils/file.ts';

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

  private static async variableDefinition(node: Block) {
    const variable = await this.process(<Element>node[0], 'Identifier');
    const value = await this.process(<Element>node[1]);
    if (typeof variable !== 'string' && 'variable' in variable) {
      const currentStackVariable = this.stack[variable.variable];
      if (typeof currentStackVariable === 'string') {
        const splitUpdate = currentStackVariable.split('');
        splitUpdate[variable.index] = value;
        this.stack[variable.variable] = splitUpdate.join('');
      }
      else currentStackVariable[variable.index] = value;
    }
    else this.stack[await this.process(<Element>node[0], 'Identifier')] = value;
  }

  private static processValue(element: Element, state?: string) {
    if (element.value === 'none') return undefined;
    if (element.value === 'stack') return this.stack;
    if (element.type === 'Word') {
      if (state && state === 'Identifier') return element.value;
      if (this.stack[element.value] !== undefined) return this.stack[element.value];
      return 'none';
    }
    return element.value as string;
  }

  private static async processArithmetic(operation: string, args: Block): Promise<any> {
    switch (operation) {
      case '+': // @ts-ignore
        return args.reduce(async (acc: any, cur: any) => (await this.process(acc)) + (await this.process(cur)));
      case '-': // @ts-ignore
        return args.reduce(async (acc: any, cur: any) => (await this.process(acc)) - (await this.process(cur)));
      case '*':
        // @ts-ignore
        return args.reduce(async (acc: any, cur: any) => (await this.process(acc)) * (await this.process(cur)));
      case '/':
        // @ts-ignore
        return args.reduce(async (acc: any, cur: any) => (await this.process(acc)) / (await this.process(cur)));
    }
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
    for (const arg of args) {
      const argumentName: string = await this.process(<Element>arg, 'Identifier');
      if (typeof argumentName === 'string') functionArguments.push({
        arg: argumentName,
        variadic: false,
      });
      else functionArguments.push(argumentName);
    }
    return {
      args: functionArguments,
      body,
      type: 'Function',
      js: false,
    };
  }

  private static async callFunction(node: Block, functionName: string) {
    const values = [];
    for (const arg of node) {
      const parsedArgument = await this.process(arg, 'Argument');
      if (!['string', 'number'].includes(typeof parsedArgument)) {
        if ('variadic' in parsedArgument) {
          values.push(...parsedArgument.value);
        } else values.push(parsedArgument);
      } else values.push(parsedArgument);
    }
    if (this.stack[functionName].js === true) return this.stack[functionName].func(...values);
    this.pushStackFrame();
    const fn = this.stack[functionName]
    for (const index in fn.args) {
      if (fn.args[index].variadic === true) this.stack[fn.args[index].arg] = values.slice(Number(index));
      else this.stack[fn.args[index].arg] = values[Number(index)];
    }
    for (const el of fn.body) {
      const res = await this.process(el);
      if (res && res[1] && res[1] === true) {
        for (const arg of fn.args) delete this.stack[arg];
        return res[0];
      }
    }
    const lastStatement = fn.body.slice(-1)[0];
    if (lastStatement && lastStatement.length > 1 && lastStatement[0].value !== 'return') return 'none';
    if (lastStatement) {
      const processed = await this.process(lastStatement);
      for (const arg of fn.args) delete this.stack[arg];
      return processed;
    }
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
    this.popStackFrame();
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
      let module = await import(finalPath);
      let namespace: string = module.namespace ? module.namespace : '';

      if (!module || !module.module) throw 'No modules found in ' + src;
      if (!Array.isArray(module.module)) module.module = [module.module];

      const name: string = namespace.length > 0 ? `${namespace}:` : '';

      for (const mod of module.module) {
        if ('func' in mod) {
          this.stack[name + mod.name] = {
            type: 'Function',
            js: true,
            func: mod.func,
          };
        } else {
          this.stack[name + mod.name] = mod.value;
        }
      }
    }
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
    if ('value' in node) return Interpreter.processValue(node, state);
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
          else if (expr.value === 'let') return await this.variableDefinition(args);
          else if (['+', '-', '/', '*'].includes(expr.value as string)) return await Interpreter.processArithmetic(expr.value as string, args);
          else if (expr.value === 'fn') return await Interpreter.functionDefinition(args);
          else if (expr.value === 'return') return await Interpreter.processReturn(args);
          else if (['=', '!=', '<', '>', '<=', '>=', 'and', 'or'].includes(expr.value as string)) return await Interpreter.processEqualities(expr.value as string, args[0], args[1]);
          else if (expr.value === 'if') return await Interpreter.processCondition(args);
          else if (expr.value === 'spread') return await Interpreter.processSpread(args);
          else if (expr.value === 'list') return await Interpreter.processList(args, state);
          else if (expr.value === 'while') return await Interpreter.processWhile(args);
          else if (expr.value === 'index') return await Interpreter.processListIndex(args, state);
          else if (Interpreter.stack[expr.value] && Interpreter.stack[expr.value].type === 'Function') return await Interpreter.callFunction(args, expr.value as string);
          const potentialValue = await this.processValue(expr);
          if (potentialValue !== 'none') return potentialValue;
          throw `Function or keyword ${expr.value} does not exists!`;
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
    this.cwd = path.dirname(src);
    await this.process(this.ast);
    return undefined;
  }
}