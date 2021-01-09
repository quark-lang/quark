import type { Block, Element } from '../typings/block.ts';
import { Parser } from './parser.ts';

export class Node {
  public static process(block: Block): void | ValueElement {
    for (const child of block) {
      let res: undefined | [ValueElement, boolean] = Interpreter.process(child);
      if (res && res[1] && res[1] === true) {
        return res[0];
      }
    }
  }
}

export class Variable {
  public static declare(variable: Element, value: Block | Element) {
    Frame.frame.variables.push({
      name: Identifier.process(variable),
      value: Interpreter.process(value) as ValueElement,
    });
  }

  public static update(variable: Element, value: Element | Block) {
    const identifier: string = Identifier.process(variable);
    const frameItem = Frame.variables.get(identifier) as ValueElement;
    if (!Frame.exists(identifier)) throw 'Variable ' + identifier + ' does not exists!';
    Value.update(frameItem, Interpreter.process(value));
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

  public static update(current: any, next: any): void {
    const loop = Object.entries(next);
    for (const item of loop) current[item[0]] = item[1];
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
  Function = 'Function'
}

interface StringType {
  type: Types.String,
  value: string,
}

interface IntegerType {
  type: Types.Integer,
  value: number,
}

interface FunctionType {
  type: Types.Function,
  args: Element[],
  body: Block,
}

type ValueElement = StringType | IntegerType | FunctionType;
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

export class Function {
  public static declare(args: (Element extends Block ? never : Element)[], body: Block): FunctionType {
    return {
      type: Types.Function,
      args,
      body,
    }
  }

  public static call(functionName: string, args: (Block | Element)[]) {
    const fn: FunctionType = Frame.variables.get(functionName) as FunctionType;
    Frame.pushStackFrame();
    for (let binding in fn.args) Variable.declare(fn.args[binding], args[Number(binding)]);
    let res: any = Interpreter.process(fn.body);
    Frame.popStackFrame();
    return res || { type: 'String', value: 'none' };
  }

  public static return(value: Block | Element): [ValueElement, boolean] {
    return [Interpreter.process(value), true];
  }
}

export class Interpreter {
  public static process(block: Block | Element) {
    if (isValue(block)) return Value.process(block as Element);
    if (isContainer(block)) {
      Frame.pushStackFrame();
      const res = Node.process(block as Block);
      Frame.popStackFrame();
      return res;
    }
    const [ expr, ...args ] = block as (Block | Element)[];
    const expression: Element = expr as Element;
    if (expression.value === 'let') return Variable.declare(args[0] as Element, args[1]);
    if (expression.value === 'set') return Variable.update(args[0] as Element, args[1]);
    if (expression.value === 'fn') return Function.declare(args[0] as Element[], args[1] as Block);
    if (expression.value === 'return') return Function.return(args[0]);
    if (expression.value === 'print') {
      const values: (ValueElement | void)[] = args.map(Interpreter.process);
      return console.log(...values.map((x: any) => x.value));
    }
    if (Frame.exists(expression.value as string)) {
      const item: ValueElement = Frame.variables.get(expression.value as string) as ValueElement;
      if (item.type === Types.Function) {
        return Function.call(expression.value as string, args);
      } else return item;
    }
    throw `Can't recognize this expression: ${expression.value}`;
  }

  public static run(source: string) {
    const ast = Parser.parse(source);
    return this.process(ast);
  }
}