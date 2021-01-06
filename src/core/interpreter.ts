import type { Block, Element } from '../typings/block.ts';
import { Parser } from './parser.ts';
import { existsSync } from 'https://deno.land/std/fs/mod.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';
import { File } from '../utils/file.ts';

type ProcessResponse = [any | undefined, boolean];
type Stack = { variables: { name: string, value: Value }[] }[];

// Values
type String = { type: 'String', value: string, };
type Integer = { type: 'Integer', value: number };
type Function = { type: 'Function', args: Value[], body: Block }

type Value = String | Integer | Function;

export class Interpreter {
  private static stack: Stack = [ { variables: [] } ];
  private static ast: Block;
  static cwd: string;

  public static parentDir(src: string, it: number = 1): string {
    for (let i = 0; i < it; i++) src = path.dirname(src);
    return src;
  }

  private static isContainer(element: any): boolean {
    return Array.isArray(element) && element.every((child) => Array.isArray(child));
  }

  private static isObject(variable: any): boolean {
    return !Array.isArray(variable) && typeof variable === 'object';
  }

  private static processIdentifier(identifier: Element): string {
    return identifier.value as string;
  }

  private static processValue(value: Element | Block): Value {
    if (Array.isArray(value)) {
      if ((<Element>value[0]).value === 'fn') {
        return this.processFunctionDefinition(<Block>value[1], <Block>value[2]);
      }
      return value as unknown as Value;
    }
    return value as Value;
  }

  private static processFunctionDefinition(args: Block, body: Block): Function {
    return {
      type: 'Function',
      args: args.map((acc) => this.processValue(acc)),
      body,
    }
  }

  private static processVariableDefinition(identifier: Element, value: Element | Block): ProcessResponse {
    const variable = this.processIdentifier(identifier);
    const currentFrame = this.stack[this.stack.length - 1];

    currentFrame.variables.push({
      name: variable,
      value: this.processValue(value),
    });

    return [undefined, false];
  }

  private static processBlock(block: Block): any {
    for (const instruction of block) {
      if (this.isContainer(instruction)) {
        return this.processBlock(instruction as Block);
      } else {
        // @ts-ignore
        const [expr, ...args] = instruction;
        const expression = (<Element>expr).value;
        let res: ProcessResponse = [undefined, false];
        if (expression === 'let') res = this.processVariableDefinition(<Element>args[0], args[1]);
        if (res[1]) {
          return res;
        }
      }
    }
  }

  public static async run(source: string, src: string) {
    this.ast = Parser.parse(source);
    this.cwd = src;
    this.processBlock(this.ast);
    return undefined;
  }
}