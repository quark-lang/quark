import {
  Interpreter,
  Frame,
  Function,
  Types,
  Variable,
  ValueElement,
  StringType,
  IntegerType, ListType, BooleanType, NoneType
} from '../src/core/interpreter.ts';
import { QuarkTypes } from './typings/types.ts';
import { QuarkCallback } from './typings/callback.ts';
import { Block } from '../src/typings/block.ts';

export interface QuarkDefinition {
  name: string,
}

export interface QuarkFunction extends QuarkDefinition {
  name: string,
  body: Function,
}

export interface QuarkVariable extends QuarkDefinition {
  name: string,
  value: ValueElement,
}

export function quarkify(fn: (...data: any[]) => any, ...args: ValueElement[]): any {
  return fn.call(fn, ...getValue(args));
}

export class QuarkType {
  public static string(value: string): StringType {
    return {
      type: Types.String,
      value,
    };
  }

  public static number(value: string | number): IntegerType {
    return {
      type: Types.Integer,
      value: Number(value),
    };
  }

  public static list(value: ValueElement[]): ListType {
    return {
      type: Types.List,
      value,
    }
  }

  public static boolean(value: any): BooleanType {
    return {
      type: Types.Boolean,
      value: Boolean(value),
    }
  }

  public static none(): NoneType {
    return {
      type: Types.None,
      value: undefined,
    }
  }
}

export function setValue(data: any[]) {
  let result: ValueElement[] = [];
  for (const el of data) {
    if (Array.isArray(el)) {
      result.push(QuarkType.list(setValue(el)));
    } else {
      if (typeof el === 'string') {
        result.push(QuarkType.string(el));
      } else if (typeof el === 'number') {
        result.push(QuarkType.number(el));
      } else if (typeof el === 'boolean') {
        result.push(QuarkType.boolean(el));
      } else result.push(QuarkType.none());
    }
  }
  return result;
}


export function getValue(values: ValueElement[]): any {
  let result: any = [];
  for (const value of values) {
    if (value.type === Types.List) {
      result.push(getValue(value.value));
    } else if ('value' in value) result.push(value.value === undefined ? 'none' : value.value);
    else result.push('none');
  }
  return result;
}

export class QuarkModule {
  public static stack = Frame.stack;
  public static cwd = Interpreter.cwd;
  public static declare(namespace: string | null, type: QuarkTypes, definition: QuarkVariable | QuarkFunction): QuarkDefinition {
    const ns: string = namespace ? `${namespace}:${definition.name}` : definition.name;
    if (type === QuarkTypes.QuarkFunction) {
      definition = <QuarkFunction>definition;
      Frame.frame.variables.push({
        name: ns,
        value: {
          type: Types.Function,
          args: [],
          js: true,
          body: definition.body as (() => {}),
        },
      });
    } else if (type === QuarkTypes.QuarkVariable) {
      definition = <QuarkVariable>definition;
      Frame.frame.variables.push({
        name: ns,
        value: definition.value,
      })
    }
    return definition;
  }
}
