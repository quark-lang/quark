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
  public static async process(node: Block) {
    for (const child of node) {
      const res: undefined | [ValueElement, boolean] = await Interpreter.process(child);
      if (res && res[1] && res[1] === true) {
        return res;
      }
    }
  }
}

export class Function {
  public static declare(args: (Element extends Block ? never : Element)[], body: Block, js: boolean = false): FunctionType | null {
    return null;
  }

  public static async call(functionName: string | FunctionType, args: (Block | Element)[]) {

  }
}

export class Interpreter {
  public static async process(node: Block | Element): Promise<any> {
    if (isValue(node)) {
      return node;
    } else if (isContainer(node)) {
      Frame.pushLocalFrame();
      await Node.process(<Block>node);
      Frame.popLocalFrame();
    } else {
      const [expr, ...args] = <Block>node;
      const expression: string = <string>(<Element>(await Interpreter.process(expr))).value;

      switch (expression) {
        case 'print':
          const _args = [];
          for (const arg of args) _args.push((await Interpreter.process(arg)).value);
          console.log(..._args);
          break;
      }
    }
  }

  public static async run(code: string, path: string) {
    const ast = Parser.parse(code);
    return await this.process(ast);
  }
}