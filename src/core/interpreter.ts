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
    console.log(Identifier.process(variable), value);
  }
}

export class Identifier {
  public static process(element: Element) {
   if ('value' in element) return element.value;
   throw 'Variable name is not correct!';
  }
}

export class Value {
  public static process(value: Element) {

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
  private static stack: Stack[];
  public static pushStackFrame() {

  }
}

export class Interpreter {
  public static process(block: Block | Element) {
    if (isValue(block)) return Value.process(block as Element);
    if (isContainer(block)) return Node.process(block as Block);
    const [ expression, ...args ] = block as (Block | Element)[];
    if (expression.value === 'let') return VariableDefinition.process(args[0], args[1]);
  }

  public static run(source: string) {
    const ast = Parser.parse(source);
    return this.process(ast);
  }
}