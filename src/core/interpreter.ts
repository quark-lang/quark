import type { Block, Element } from '../typings/block.ts';
import { Parser } from './parser.ts';
import { existsSync } from 'https://deno.land/std/fs/mod.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';
import { File } from '../utils/file.ts';
import { getQuarkFolder } from '../main.ts';
import { 
  isContainer,
  isValue,
  isObject,
  parentDir
} from '../utils/runner.ts';
import {
  Types,
  ValueElement,
  ListType,
  IntegerType,
  StringType,
  NoneType,
  BooleanType,
  FunctionType,
  Argument
} from '../typings/types.ts';

export type Stack = [FunctionFrame];
export type FunctionFrame = [LocalFrame];
export type LocalFrame = { name: string, value: ValueElement }[];

const paths: string[] = [];

export class Frame {
  public static stack: Stack = [[[]]];

  public static pushFunctionFrame() {
    this.stack.push([[]]);
  }
  public static popFunctionFrame() {
    this.stack.pop();
  }

  public static pushLocalFrame() {
    this.stack.slice(-1)[0].push([]);
  }
  public static popLocalFrame() {
    this.stack.slice(-1)[0].pop();
  }

  public static get global(): FunctionFrame {
    return this.stack[0];
  }
  public static get local(): LocalFrame {
    return this.stack.slice(-1)[0][0];
  }

  public static exists(identifier: string) {
    return this.variables.get(identifier) || false;
  }
  public static get variables() {
    let map = new Map();
    for (const frame of this.global) {
      for (const variable of frame) {
        map.set(variable.name, variable.value);
      }
    }
    for (const frame of this.stack.slice(-1)[0]) {
      for (const variable of frame) {
        map.set(variable.name, variable.value);
      }
    }
    return map;
  }
}

export class Node {
  public static async process(node: Block, global: boolean) {
    for (const child of node) {
      const res: undefined | [ValueElement, boolean] = await Interpreter.process(child, global);
      if (res && res[1] && res[1] === true) {
        return res;
      }
    }
  }
}

export class Function {
  public static declare(args: (Element extends Block ? never : Element)[], body: Block): FunctionType {
    return {
      type: Types.Function,
      args: args as Argument[],
      body,
    };
  }

  public static async call(functionName: string | FunctionType, args: (Block | Element)[]) {
    const func = Frame.variables.get(functionName);
    Frame.pushFunctionFrame();
    for (const index in args) {
      const correspondent = func.args[index];
      await Variable.declare(correspondent, await Interpreter.process(args[index]), false);
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
  public static async declare(identifier: Atom, value: Atom, global: boolean) {
    const _id = (<Element>identifier).value;
    if (Frame.exists(<string>_id) === false) {
      if (global === true) {
        Frame.global[0].push({
          name: <string>_id,
          value: await Interpreter.process(value),
        });
        return;
      }
      Frame.local.push({
        name: <string>_id,
        value: await Interpreter.process(value),
      });
    }
  }

  public static async update(identifier: Atom, value: Atom) {
    const _id = (<Element>identifier).value;
    if (Frame.exists(<string>_id)) {
      Value.update(Frame.variables.get(_id), await Interpreter.process(value));
    }
  }
}

export class Value {
  public static async get(element: Element) {
    if (element.type === 'Word') {
      if (Frame.exists(<string>element.value) === false) {
        if (<string>element.value === 'print') return element;
        return { type: 'None', value: undefined };
      } else {
        return Frame.variables.get(element.value);
      }
    }
    return { ...element };
  }

  public static update(current: any, next: any): void {
    for (const item of Object.entries(next))
      current[item[0]] = item[1];
  }
}

export class Interpreter {
  public static async process(node: Atom, global: boolean = false): Promise<any> {
    if (isValue(node)) {
      return await Value.get(<Element>node);
    } else if (isContainer(node)) {
      Frame.pushLocalFrame();
      const res = await Node.process(<Block>node, global);
      Frame.popLocalFrame();
      return res;
    } else {
      const [expr, ...args] = <Block>node;
      const expression: string = <string>(<Element>expr).value;

      switch (expression) {
        case 'print':
          const _args = [];
          for (const arg of args) _args.push((await Interpreter.process(arg)).value);
          return console.log(..._args);

        case 'let': return await Variable.declare(args[0], args[1], global);
        case 'set': return await Variable.update(args[0], args[1]);
        case 'fn': return Function.declare(<Element[]>args[0], <Block>args[1]);
        case 'import': return await Import.process(args[0]);
        case 'return': return await Function.return(args[0]);
      }

     if (Frame.exists(expression)) {
       const variable = <ValueElement>Frame.variables.get(expression);
       if (variable.type === 'Function') {
         return await Function.call(expression, args);
       }
     }
    }
  }

  public static async run(code: string, path: string) {
    const ast = Parser.parse(code);
    paths.push(path);
    await this.process(ast);
    paths.pop();
  }
}