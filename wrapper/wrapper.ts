/*
  C Wrapper for Quark
  Written by Thomas
*/

export namespace Wrapper {
  const output: string[] = [];

  export function include(library: string) {
    output.push(`#include <${library}>`);
  }

  export namespace Variable {
    export interface Variable {
      type: string,
      name: string,
      value: any,
    }
    export function define(name: string, type: string, value: any, array: (number | undefined)[] = []): Variable {
      output.push(`${type} ${name}${array.map((acc) => `[${acc || ''}]`)} = ${value};`);
      return {
        type,
        name,
        value,
      };
    }
  }

  export namespace Value {
    export interface Value {
      type: string,
      value: any,
      isVariable?: boolean,
    }

    function string(value: string) {
      return `"${value}"`;
    }
    function array(value: any[]) {
      let result: string = '{';
      for (const index in value) {
        const item = value[index];
        if (Array.isArray(item)) {
          result += array(item);
        } else {
          result += JSON.stringify(item);
        }
        if (Number(index) !== value.length - 1) result += ',';
      }
      return result + '}';
    }

    export function get(value: Value) {
      if (typeof Number(value.value) === 'number' && !isNaN(Number(value.value)))
        return value.value;
      if (Array.isArray(value.value))
        return array(value.value);
      if (value.isVariable)
        return value.value;
      if (typeof value.value === 'string')
        return string(value.value);
    }
  }

  export namespace List {
    export function getIndex(variable: string, index: number): Wrapper.Value.Value {
      return {
        type: 'void*',
        value: `${variable}[${index}]`,
      };
    }
  }

  export namespace Function {
    export interface Argument {
      type: string,
      name: string,
    }
    export function define(name: string, type: string, args: Wrapper.Function.Argument[], body: () => void) {
      const parsedArguments = args.map((arg) => `${arg.type} ${arg.name}`);
      output.push(`${type} ${name}(${parsedArguments.join(', ')}) {`);
      body();
      output.push('}');
    }
    export function call(name: string, args: Wrapper.Value.Value[], scoped: boolean = false) {
      const parsedArguments = args.map((arg) => arg.value);
      output.push(`${name}(${parsedArguments.join(', ')})${scoped ? '' : ';'}`);
    }
  }

  export function print() {
    console.log(output.join('\n'));
  }
}