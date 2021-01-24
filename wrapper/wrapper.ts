/*
  C Wrapper for Quark
  Written by Thomas
*/
import { Types as Primitives } from '../src/typings/types.ts';

export namespace Wrapper {
  enum Miscellaneous {
    Word = 'Word'
  }
  type Types = Primitives | Miscellaneous;

  export const output: string[] = [];

  export interface Value<T> {
    type: T,
    value: any,
  }

  export function get<T>(value: Value<T>) {
    const type = <string><unknown>value.type;
    switch (type) {
      case 'String':
        return `"${value.value}"`;
      case 'Integer':
        return Number(value.value);
      case 'Word':
        return value.value;
    }
  }

  export namespace Variable {
    export function define<T extends Types>(name: string, value: Value<T>) {
      output.push(`let ${name} = ${get(value)};`);
    }
  }

  export namespace Function {
    export function call(name: string, args: Value<Types>[], scoped: boolean = false) {
      output.push(`${name}(${args.map((arg) => get<any>(arg)).join(', ')})${scoped ? '' : ';'}`)
    }
  }

  export function print(join = '\n'): string {
    console.log(output.join(join));
    return output.join(join);
  }
}