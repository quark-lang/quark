import type { Block, Element } from '../typings/block.ts';
import { Parser } from './parser.ts';
import { existsSync } from "https://deno.land/std/fs/mod.ts";
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';
import { File } from '../utils/file.ts';
import { isContainer, isObject, isValue, parentDir } from '../utils/runner.ts';
import { Argument, FunctionType, ListType, Types, ValueElement } from '../typings/types.ts';
import { getQuarkFolder } from '../main.ts';

export type Stack = [FunctionFrame];
export type FunctionFrame = [LocalFrame];
export type LocalFrame = { name: string, value: ValueElement }[];

export const paths: string[] = [];

export class Frame {
  public static stack: Stack = [[[]]];
  public static init() {
    this.stack = this.stack.slice(0, 1) as Stack;
  }

  public static pushFunctionFrame() {
    this.stack.push([[]]);
  }
  public static popFunctionFrame() {
    this.stack.pop();
  }

  public static pushLocalFrame(variables?: LocalFrame) {
    this.stack.slice(-1)[0].push(variables || []);
  }
  public static popLocalFrame() {
    this.stack.slice(-1)[0].pop();
  }

  public static get global(): FunctionFrame {
    return this.stack[0];
  }
  public static get local(): LocalFrame {
    return this.stack.slice(-1)[0].slice(-1)[0];
  }
  public static get frame(): FunctionFrame {
    return this.stack.slice(-1)[0];
  }

  public static exists(identifier: string) {
    return this.variables().get(identifier) || false;
  }
  public static variables(stack?: FunctionFrame) {
    let map = new Map();
    for (const frame of this.global) {
      for (const variable of frame) {
        map.set(variable.name, variable.value);
      }
    }
    for (const frame of stack || this.frame) {
      for (const variable of frame) {
        map.set(variable.name, variable.value);
      }
    }
    return map;
  }
}

export class Node {
  public static async process(node: Block) {
    for (const child of node) {
      const res: undefined | [ValueElement, boolean] = await Interpreter.process(child);
      if (res && res[1] && res[1] === true) {
        return res;
      }
    }
  }
}

export class List {
  public static async create(args: Atom[]): Promise<ListType> {
    const value = []
    for (const arg of args) {
      const processed = await Interpreter.process(arg);
      value.push(processed);
    }
    return { type: Types.List, value, };
  }

  public static async index(variable: Element, index: Element): Promise<any> {
    const element = await Interpreter.process(variable);
    index = await Interpreter.process(index);
    if (element.type === Types.Function) return { type: Types.None, value: undefined };
    if ('value' in element && element.value !== undefined) { // @ts-ignore
      const foundIndex = element.value[index.value];
      if (typeof foundIndex === 'string') return { type: element.type, value: foundIndex };
      return foundIndex || { variable: variable.value, index: index.value };
    }
    return { type: Types.None, value: undefined };
  }
}

export class Function {
  public static declare(args: (Element extends Block ? never : Element)[], body: Block): FunctionType {
    return {
      type: Types.Function,
      args: args as Argument[],
      closure: <FunctionFrame>Frame.frame.concat(Frame.global),
      js: false,
      body,
    };
  }

  public static async call(functionName: string | FunctionType, args: (Block | Element)[]) {
    const func = Frame.variables().get(functionName);

    // Simply processing function as a js function
    if (func.js === true) {
      const _args = [];
      for (const index in args) {
        const arg = args[index];
        const processed = await Interpreter.process(arg);
        _args.push(processed);
      }
      return await func.body(..._args);
    }

    // Processing argument values in order to stock them after
    const _args = [];
    for (const index in args) {
      const correspondent = func.args[index];
      const processed = await Interpreter.process(args[index]);
      _args.push([correspondent, processed]);
    }

    // Pushing new private frame corresponding to function scope with function env
    Frame.pushFunctionFrame();
    Frame.pushLocalFrame(func.closure.flat());

    // Declaring call arguments with before parsed arguments
    for (const arg of _args) {
      const [variable, value] = arg;
      await Variable.declare(variable, value);
    }

    const res = await Interpreter.process(func.body);
    Frame.popFunctionFrame();
    if (res && res[1] && res[1] === true) return res[0];
    return res || { type: 'None', value: undefined };
  }

  public static async return(value: Atom): Promise<[ValueElement, boolean]> {
    return [await Interpreter.process(value), true];
  }
}

export type Atom = Element | Block;

export class Variable {
  public static async declare(identifier: Atom, value: Atom) {
    if (!identifier) return;
    const _id = (<Element>identifier).value;
    if (Frame.local.find((acc) => acc.name === _id) === undefined) {
      Frame.local.push({
        name: <string>_id,
        value: await Interpreter.process(value),
      });
    } else {
      await Variable.update(identifier, value);
    }
  }

  public static async update(identifier: Atom, value: Atom) {
    const _id = await Identifier.process(<Element>identifier);

    if (typeof _id === 'string') {
      const frameItem = Frame.variables().get(_id) as ValueElement;
      if (!Frame.exists(_id)) throw 'Variable ' + _id + ' does not exists!';
      return Value.update(frameItem, await Interpreter.process(value));
    }
    if ('index' in _id && 'variable' in _id) {
      const variable = Frame.variables().get((<any>_id).variable) as ValueElement;
      if ('value' in variable && variable.value !== undefined) {
        const updateValue = await Interpreter.process(value);
        if (variable.type === Types.List) {
          variable.value[(<any>_id).index] = updateValue;
        } else {
          const split = (<string>variable.value).split('');
          split.splice((<any>_id).index, updateValue.value.length, updateValue.value)
          variable.value = split.join('');
        }
      }
    }
    return Value.update(_id, await Interpreter.process(value));

  }
}

export class Identifier {
  public static async process(element: Element): Promise<string | { variable: any, index: any, }> {
    if (Array.isArray(element) && element[0].type === 'Word' && element[0].value === 'index') {
      const index = await List.index(element[1], element[2]);
      if (isObject(index))
        return index;
      return { variable: element[1].value, index: element[2].value };
    }
    if ('value' in element)
      return element.value as string;
    return (await Interpreter.process(element)).value;
  }
}

export class Value {
  public static async get(element: Element) {
    if (element.type === 'Word') {
      if (Frame.exists(<string>element.value) === false) {
        if (<string>element.value === 'print') return element;
        return { type: 'None', value: undefined };
      } else {
        return Frame.variables().get(element.value);
      }
    } else if (element.value === 'none') {
      return {
        type: 'None',
        value: undefined,
      }
    }
    return { ...element };
  }

  public static update(current: any, next: any): void {
    for (const item of Object.entries(next))
      current[item[0]] = item[1];
  }
}

export class Import {
  public static async process(mod: Atom) {
    const file = await Interpreter.process(mod);
    const src = path.isAbsolute(file.value)
      ? ''
      : parentDir(paths.slice(-1)[0]);

    // Deducing STD module path
    const root: string = await getQuarkFolder();
    const std: string = path.join(root, 'std');

    // Setting all possible module paths
    const stdMod: string = path.join(std, file.value);
    const modulePath: string = path.join(src, file.value);

    // Setting final path to existing module
    const finalPath = existsSync(modulePath)
      ? modulePath
      : existsSync(stdMod)
        ? stdMod
        : undefined

    if (finalPath === undefined) throw `Module "${file.value}" does not exists!`;
    const content: string = await File.read(finalPath);

    // Interpreting module and merging module global frame to current local frame
    const res = await Interpreter.run(content, finalPath, true);
    Frame.frame.concat(<FunctionFrame><unknown>res);
  }
}

export function getValue(values: ValueElement[]): any {
  let result: any = [];
  for (const value of values) {
    if (typeof value !== 'object') result.push(value);
    else if (value.type === Types.List) {
      result.push(getValue(value.value));
    } else if ('value' in value) result.push(value.value === undefined ? 'none' : value.value);
    else result.push('none');
  }
  return result;
}

export class Condition {
  public static async process(condition: Atom, then: Atom, otherwise: Atom) {
    const _condition = await Interpreter.process(condition);
    if (_condition === true || (_condition && _condition.value === true)) {
      return await Interpreter.process(then);
    } else if (otherwise) {
      return await Interpreter.process(otherwise);
    }
  }
}

export class While {
  public static async process(condition: Atom, body: Atom) {
    while ((await Interpreter.process(condition))?.value) {
      const res = await Interpreter.process(body);
      if (res) return [res, true];
    }
  }
}

export class Interpreter {
  public static async process(node: Atom): Promise<any> {
    if (node === undefined) return { type: 'None', value: undefined };
    if ('index' in node) {
      return node;
    } else if (isValue(node)) {
      return await Value.get(<Element>node);
    } else if (isContainer(node)) {
      Frame.pushLocalFrame();
      const res = await Node.process(<Block>node);
      Frame.popLocalFrame();
      return res;
    } else if (Array.isArray(node)) {
      const [expr, ...args] = <Block>node;
      const expression: string = <string>(<Element>expr).value;

      switch (expression) {
        case 'let': return await Variable.declare(args[0], args[1]);
        case 'set': return await Variable.update(args[0], args[1]);
        case 'fn': return Function.declare(<Element[]>args[0], <Block>args[1]);
        case 'import': return await Import.process(args[0]);
        case 'return': return await Function.return(args[0]);
        case 'list': return await List.create(args);
        case 'index': return await List.index(<Element>args[0], <Element>args[1]);
        case 'if': return await Condition.process(args[0], args[1], args[2]);
        case 'while': return await While.process(args[0], args[1]);
      }

     if (Frame.exists(expression)) {
       const variable = <ValueElement>Frame.variables().get(expression);
       if (variable.type === 'Function') {
         return await Function.call(expression, args);
       }
       return variable;
     } else {
       throw `Function "${expression}" does not exists!`;
     }
    }
  }

  public static async run(code: string, src: string, module?: boolean) {
    let _ast = Parser.parse(code);
    paths.push(src);
    Frame.init();
    if (module === true) {
      if (Array.isArray(_ast) && _ast.length === 1 && Array.isArray(_ast[0])) {
        _ast = _ast[0];
      }
    }
    for (const branch of _ast) {
      await this.process(branch);
    }
    paths.pop();
    return Frame.stack;
  }
}