import type { Block, Element } from '../typings/block.ts';
import { Parser } from './parser.ts';
import { existsSync } from 'https://deno.land/std/fs/mod.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';
import { File } from '../utils/file.ts';

type ProcessResponse = [any | undefined, boolean];
type StackVariable = { name: string, value: Value };
type Stack = { variables: StackVariable[] }[];

// Values
type String = { type: 'String', value: string, };
type Integer = { type: 'Integer', value: number };
type Function = { type: 'Function', args: string[], body: Block }

type Value = String | Integer | Function;

export class Interpreter {
  private static stack: Stack = [ { variables: [] } ];
  private static ast: Block;
  static cwd: string;

  private static pushStackFrame() {
    this.stack.push({ variables: [] });
  }

  private static popStackFrame() {
    this.stack.pop();
  }

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
      args: args.map((acc) => this.processValue(acc).value),
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

  private static get getStackVariables(): Map<any, any> {
    let map = new Map();
    for (const index in this.stack) {
      const frame = this.stack[index];
      for (const variable of frame.variables) map.set(variable.name, { scope: Number(index), value: variable.value });
    }
    return map;
  }

  private static processVariableUpdate(identifier: Element, value: Element | Block): ProcessResponse {
    const variable = this.processIdentifier(identifier);
    const stackElement = this.getStackVariables.get(variable);

    const stackVariables = this.stack[stackElement.scope].variables;
    const foundVariable = stackVariables.find((acc) => acc.name === variable);
    if (!foundVariable) throw 'Variable does not exists!';
    foundVariable.value = this.processValue(value);

    return [undefined, false];
  }

  private static getValue(element: Element | Block): Value | 'none' | Element {
    if (Array.isArray(element)) {
      const [expr, ...args] = element;
      const expression = (<Element>expr).value;
      const stackElement = this.getStackVariables.get(expression);
      if (!stackElement) return 'none';
      if (stackElement.value.type === 'Function') {
        const res = this.processFunctionCall(stackElement.value, args);
        if (res) return res;
        return 'none';
      }
      else return 'none';
    }
    if (element.type !== 'Word') return element;
    const stackElement = this.getStackVariables.get(element.value);
    if (!stackElement) return 'none';
    return stackElement.value;
  }

  private static processPrint(args: Block): ProcessResponse {
    const processedArguments = args.map((arg) => this.getValue(arg));
    console.log(...processedArguments.map((acc) => acc.value || 'none'));
    return [undefined, false]
  }

  private static processFunctionCall(func: Function, args: Block) {
    const bindings = func.args.map((name, i) => (
      {
        name,
        value: this.getValue(args[i]),
      }
    ));
    this.pushStackFrame();
    this.stack[this.stack.length - 1].variables.push(...bindings as StackVariable[]);
    let returnValue = this.processBlock(func.body);
    this.popStackFrame();
    return returnValue[0];
  }

  private static processReturn(arg: Block): ProcessResponse {
    return [this.getValue(arg), true];
  }

  private static processBlock(block: Block): any {
    for (const instruction of block) {
      if (this.isContainer(instruction)) {
        this.pushStackFrame();
        this.processBlock(instruction as Block);
        this.popStackFrame();
      } else {
        // @ts-ignore
        const [expr, ...args] = instruction;
        const expression = (<Element>expr).value;
        let res: ProcessResponse = [undefined, false];

        if (expression === 'let') res = this.processVariableDefinition(<Element>args[0], args[1]);
        else if (expression === 'set') res = this.processVariableUpdate(<Element>args[0], args[1]);
        else if (expression === 'print') res = this.processPrint(args);
        else if (expression === 'return') res = this.processReturn(args[0] as Block);
        const potentialVariable = this.getStackVariables.get(expression);
        if (potentialVariable && potentialVariable.value.type === 'Function') res = this.processFunctionCall(potentialVariable.value, args);
        if (res[1] === true) return res;
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