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

  const output: string[] = [];

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
    }
  }

  export namespace Variable {
    export function define<T extends Types>(name: string, value: Value<T>) {
      output.push(`let ${name} = ${get(value)};`);
    }
  }

  export function print(join = '') {
    console.log(output.join(join));
  }
}