import type { Block, Element } from '../typings/block.ts';
import { Parser } from './parser.ts';
import { existsSync } from 'https://deno.land/std/fs/mod.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';
import { File } from '../utils/file.ts';

let count = 0;

export class Node {
  public static async process(block: Block, cwd: string, global: boolean = false): Promise<[ValueElement, boolean] | void> {
    for (const child of block) {
      if (isContainer(child) && global === true ) {
        if (count > 0) global = false;
        count++;
      }
      let res: undefined | [ValueElement, boolean] = await Interpreter.process(child, cwd, global);
      if (res && res[1] && res[1] === true) {
        return res;
      }
    }
  }
}

export class Variable {
  public static async declare(variable: Element, value: Block | Element, global: boolean = false) {
    if (global && global === true) {
      Frame.global.variables.push({
        name: await Identifier.process(variable) as string,
        value: await Interpreter.process(value) as ValueElement,
      });
    } else {
      Frame.frame.variables.push({
        name: await Identifier.process(variable) as string,
        value: await Interpreter.process(value) as ValueElement,
      });
    }
  }

  public static async update(variable: Element, value: Element | Block): Promise<void> {
    let identifier = await Identifier.process(variable);
    if (typeof identifier === 'string') {
      const frameItem = Frame.variables.get(identifier) as ValueElement;
      if (!Frame.exists(identifier)) throw 'Variable ' + identifier + ' does not exists!';
      return Value.update(frameItem, await Interpreter.process(value));
    }
    if ('index' in identifier) {
      const variable = Frame.variables.get(identifier.variable) as ValueElement;
      if ('value' in variable && variable.value !== undefined) {
        const updateValue = await Interpreter.process(value);
        if (variable.type === Types.List) {
          variable.value[identifier.index] = updateValue;
        } else {
          const split = (<string>variable.value).split('');
          split.splice(identifier.index, updateValue.value.length, updateValue.value)
          variable.value = split.join('');
        }
      }
    }
    return Value.update(identifier, Interpreter.process(value));
  }
}

export class Identifier {
  public static async process(element: Element): Promise<string | { variable: any, index: any, }> {
    if (Array.isArray(element) && element[0].type === 'Word' && element[0].value === 'index') {
      const index = await List.index(element[1], element[2]);
      if (isObject(index)) {
        return index;
      }
      return { variable: element[1].value, index: element[2].value };
    }
    if ('value' in element) return element.value as string;
    throw 'Variable name is not correct!';
  }
}

export class Value {
  public static process(value: Element): ValueElement extends FunctionType ? never : ValueElement {
    if (value.value === 'none') return { type: Types.None, value: undefined };
    if (['true', 'false'].includes(value.value as string)) return { type: Types.Boolean, value: Boolean(value.value) };
    if (['Word', 'Function'].includes(value.type) && Frame.exists(value.value as string)) {
      const variable: ValueElement = Frame.variables.get(value.value as string) as ValueElement;
      return variable.type === Types.Function ? { type: Types.None, value: undefined } : variable;
    }
    if ((value.type as string) === 'Function') {
      return { ...value } as ValueElement;
    }
    return value.value !== undefined ? { ...value } as ValueElement : { type: Types.None, value: undefined };
  }

  public static update(current: any, next: any): void {
    const loop = Object.entries(next);
    for (const item of loop) current[item[0]] = item[1];
  }
}

export function isContainer(element: Block | Element): boolean {
  return Array.isArray(element) && element.every((child) => Array.isArray(child));
}

function isValue(element: Block | Element): boolean {
  return element && ('value' in element || 'body' in element) && 'type' in element;
}

function isObject(element: any): boolean {
  return !Array.isArray(element) && typeof element === 'object';
}

export function parentDir(src: string, it: number = 1): string {
  for (let i = 0; i < it; i++) src = path.dirname(src);
  return src;
}

export enum Types {
  String = 'String',
  Integer = 'Integer',
  Function = 'Function',
  Boolean = 'Boolean',
  None = 'None',
  List = 'List',
}

export interface ListType {
  type: Types.List,
  value: ValueElement[],
}

export interface StringType {
  type: Types.String,
  value: string,
}

export interface NoneType {
  type: Types.None,
  value: undefined,
}

export interface IntegerType {
  type: Types.Integer,
  value: number,
}

export interface FunctionType {
  type: Types.Function,
  args: Argument[],
  body: Block | (() => {}),
  js: boolean,
}

export interface BooleanType {
  type: Types.Boolean,
  value: boolean,
}

export interface Argument extends Element {
  variadic: boolean,
}

export type ValueElement = StringType | IntegerType | FunctionType | BooleanType | NoneType | ListType;
interface Stack {
  variables: {
    name: string,
    value: ValueElement,
  }[]
}

export class Frame {
  public static stack: Stack[] = [{ variables: [] }];
  public static pushStackFrame(): void {
    this.stack.push({
      variables: [],
    });
  }

  public static get global(): Stack extends null | undefined ? never : Stack {
    return this.stack[0];
  }

  public static get frame(): Stack extends null | undefined ? never : Stack {
    return this.stack[this.stack.length - 1];
  }

  public static popStackFrame(): void {
    this.stack.pop();
  }

  public static exists(identifier: string) {
    return this.variables.get(identifier);
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

async function processVariadicSpread(args: (List | Argument)[]) {
  const processed = [];
  for (const arg of args) {
    if (!(arg instanceof List) && arg.variadic) {
      // @ts-ignore
      processed.push(...arg.value)
      break;
    }
    processed.push(arg);
  }
  return processed;
}

export class Function {
  public static declare(args: (Element extends Block ? never : Element)[], body: Block, js: boolean = false): FunctionType {
    args = args.map((arg) => {
      if (Array.isArray(arg) && arg[0].value === 'list') {
        return {
          variadic: true,
          ...arg[1],
        }
      }
      return {
        variadic: false,
        ...arg,
      };
    });
    return {
      type: Types.Function,
      args: args as Argument[],
      js,
      body,
    }
  }

  public static async call(functionName: string | FunctionType, args: (Block | Element)[]) {
    const fn: FunctionType = isObject(functionName) ? functionName as FunctionType : Frame.variables.get(functionName as string) as FunctionType;
    if (fn.js === true) {
      const values = [];
      for (const arg of args) values.push(await Interpreter.process(arg));
      return (<(...args: any[]) => {}><unknown>fn.body)(...values);
    }
    Frame.pushStackFrame();
    for (let binding in fn.args) {
      const fnArgument = fn.args[binding];
      await Variable.declare(fnArgument, await Interpreter.process(args[Number(binding)]))
    }
    let res: any = await Interpreter.process(<Block>fn.body);
    Frame.popStackFrame();
    return Array.isArray(res) ? res[0] : res || { type: Types.None, value: undefined };
  }

  public static async return(value: Block | Element): Promise<[ValueElement, boolean]> {
    return [await Interpreter.process(value), true];
  }
}

export class Equalities {
  public static async process(operation: string, left: Block | Element, right: Block | Element): Promise<BooleanType> {
    const processedLHS = await Interpreter.process(left);
    const processedRHS = await Interpreter.process(right);

    const lhs = processedLHS.type === Types.List ? JSON.stringify(processedLHS.value) : processedLHS.value;
    const rhs = processedRHS.type === Types.List ? JSON.stringify(processedRHS.value) : processedRHS.value;

    switch (operation) {
      case '<': return { type: Types.Boolean, value: lhs < rhs };
      case '>': return { type: Types.Boolean, value: lhs > rhs };
      case '<=': return { type: Types.Boolean, value: lhs <= rhs };
      case '>=': return { type: Types.Boolean, value: lhs >= rhs };
      case '=': return { type: Types.Boolean, value: lhs == rhs };
      case '!=': return { type: Types.Boolean, value: lhs != rhs };
      case 'and': return { type: Types.Boolean, value: lhs && rhs };
    }
    return { type: Types.Boolean, value: false, }
  }
}

export class While {
  public static async process(condition: Block | Element, body: Block | Element): Promise<[ValueElement, boolean] | void> {
    while ((await Interpreter.process(condition)).value) {
      const res = await Interpreter.process(body);
      if (res) return [res, true];
    }
  }
}

export class Arithmetic {
  private static determineType(lhs: ValueElement, rhs: ValueElement, operation: string): Types.Integer | Types.String {
    if (operation === '+') {
      if (lhs.type === Types.String || rhs.type === Types.Integer) return Types.String;
    }
    return Types.Integer;
  }

  public static async process(operation: string, left: Block | Element, right: Block | Element): Promise<StringType | IntegerType | NoneType> {
    const lhs: Exclude<ValueElement, FunctionType | BooleanType> = await Interpreter.process(left);
    const rhs: Exclude<ValueElement, FunctionType | BooleanType> = await Interpreter.process(right);
    const type: Types = this.determineType(lhs, rhs, operation);

    switch (operation) {
      case '+': return { type, value: <any>lhs.value + rhs.value };
      case '-': return { type, value: <any>lhs.value - <any>rhs.value } as IntegerType;
      case '*': return { type, value: <any>lhs.value * <any>rhs.value } as IntegerType;
      case '/': return { type, value: <any>lhs.value / <any>rhs.value } as IntegerType;
    }

    return { type: Types.None, value: undefined };
  }
}

export class Condition {
  public static async check(condition: Element | Block, then: Element | Block, or: Element | Block): Promise<any> {
    if ((await Interpreter.process(condition)).value) return await Interpreter.process(then);
    return await Interpreter.process(or);
  }
}

export class Import {
  public static async import(url: Element) {
    let src: string | URL = (url.value as string).replace(/:/g, '/');
    // Coming 3 folders back due to Interpreter path
    const stdPath: string = path.join('std');
    // Checking if file exists and setting correct path
    let finalPath: string | undefined = undefined;
    if (existsSync(path.join(stdPath, src))) finalPath = path.join(stdPath, src);
    else if (existsSync(path.join(rootCWD, src))) finalPath = path.join(rootCWD, src);
    if (!finalPath) throw 'Request module file ' + src + ' not found!';

    // Getting file extension for choosing which parsing to use
    const ext: string = path.extname(finalPath);
    if (ext.endsWith('.qrk')) {
      count = 0;
      const content: string = await File.read(finalPath);
      await Interpreter.process(Parser.parse(content, true), parentDir(finalPath), true);
    }

    if (['.js', '.ts'].includes(ext)) {
      throw `Can't resolve Typescript modules outside interpreter!`;
    }
  }
}

let rootCWD: string;
export class List {
  public static async create(args: (Element | Block)[]): Promise<ListType> {
    const value = []
    for (const arg of args) value.push(await Interpreter.process(arg));
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

  public static async spread(block: Block): Promise<Argument> {
    return {
      ...(await Interpreter.process(block)),
      variadic: true,
    }
  }
}

export class Interpreter {
  public static cwd: string = rootCWD;
  public static async process(block: Block | Element, cwd: string = rootCWD, global?: boolean): Promise<any> {
    if (isValue(block)) return Value.process(block as Element);
    if (isContainer(block)) {
      Frame.pushStackFrame();
      const res = await Node.process(block as Block, cwd, global);
      Frame.popStackFrame();
      return res;
    }
    if (block === undefined) return { type: Types.None, value: undefined };
    const [ expr, ...args ] = block as (Block | Element)[];
    const expression: Element = expr as Element;

    if (expression.value === 'let') return await Variable.declare(args[0] as Element, args[1], global);
    if (expression.value === 'set') return await Variable.update(args[0] as Element, args[1]);
    if (expression.value === 'fn') return Function.declare(args[0] as Element[], args[1] as Block);
    if (expression.value === 'if') return await Condition.check(args[0], args[1], args[2]);
    if (expression.value === 'return') return await Function.return(args[0]);
    if (expression.value === 'while') return await While.process(args[0], args[1]);
    if (expression.value === 'list') return await List.create(args);
    if (expression.value === 'import') return await Import.import(args[0] as Element);
    if (expression.value === 'index') return await List.index(args[0] as Element, args[1] as Element);
    if (expression.value === 'spread') return await List.spread(args[0] as Block);
    if (['<', '=', '!=', '>', '<=', '>=', 'and'].includes(expression.value as string)) return await Equalities.process(expression.value as string, args[0], args[1]);
    if (['+', '-', '*', '/'].includes(expression.value as string)) return await Arithmetic.process(expression.value as string, args[0], args[1]);

    if (Frame.exists(expression.value as string)) {
      const item: ValueElement = Frame.variables.get(expression.value as string) as ValueElement;
      if (item.type === Types.Function) return await Function.call(expression.value as string, args);
      return item;
    }
    if ([Types.String, Types.Integer, Types.Boolean].includes(expression.type as Types)) return expression;

    throw `Can't recognize this expression: ${expression.value}`;
  }

  public static async run(source: string, src: string) {
    count = 0;
    const ast = Parser.parse(source);
    rootCWD = path.dirname(src);
    return await this.process(ast);
  }
}