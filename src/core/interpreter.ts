import type { Block, Element } from '../typings/block.ts';
import { Parser } from './parser.ts';
import { existsSync } from 'https://deno.land/std/fs/mod.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';
import { File } from '../utils/file.ts';

export class Node {
  public static process(block: Block) {
    for (const child of block) {
      Interpreter.process(child);
    }
  }
}

export class VariableDefinition {
  public static process(variable: Element, value: Block | Element) {
    Frame.frame.variables.push({
      name: Identifier.process(variable),
      value: Interpreter.process(value) as ValueElement,
    });
  }
}

export class Identifier {
  public static process(element: Element): string {
   if ('value' in element) return element.value as string;
   throw 'Variable name is not correct!';
  }
}

export class Value {
  public static process(value: Element): ValueElement {
    if (value.type === 'Word' && Frame.variables.get(value.value as string)) {
      return Frame.variables.get(value.value as string) as ValueElement;
    }
    return value as ValueElement;
  }
}

function isContainer(element: Block | Element): boolean {
  return Array.isArray(element) && element.every((child) => Array.isArray(child));
}

function isValue(element: Block | Element): boolean {
  return element && 'value' in element && 'type' in element;
}

enum Types {
  String = 'String',
  Integer = 'Integer',
}

interface String {
  type: Types.String,
  value: string,
}

interface Integer {
  type: Types.Integer,
  value: number,
}

type ValueElement = String | Integer;
interface Stack {
  variables: {
    name: string,
    value: ValueElement,
  }[]
}

export class Frame {
  private static stack: Stack[] = [{ variables: [] }];
  public static pushStackFrame(): void {
    this.stack.push({
      variables: [],
    });
  }

  public static get frame(): Stack extends null | undefined ? never : Stack {
    return this.stack[this.stack.length - 1];
  }

  public static popStackFrame(): void {
    this.stack.pop();
  }

  public static get variables(): Map<string, Value> {
    let map = new Map();
    for (const frame of this.stack) {
      for (const variable of frame.variables) {
        map.set(variable.name, variable.value);
      }
    }
    return map;
  }
}

export class Interpreter {
  public static process(block: Block | Element) {
    if (isValue(block)) return Value.process(block as Element);
    if (isContainer(block)) {
      Frame.pushStackFrame();
      let res = Node.process(block as Block)
      Frame.popStackFrame();
      return res;
    }
    const [ expr, ...args ] = block as (Block | Element)[];
    const expression: Element = expr as Element;
    if (expression.value === 'let') return VariableDefinition.process(args[0] as Element, args[1]);
    if (expression.value === 'print') {
      const values = args.map(Interpreter.process);
      console.log(...values.map((x: any) => x.value));
    }
  }

  public static run(source: string) {
    const ast = Parser.parse(source);
    return this.process(ast);
  }
}