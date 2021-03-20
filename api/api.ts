import {
  Interpreter,
  Frame,
  Function,
} from '../src/core/interpreter.ts';
import {
  Types,
  ValueElement,
  ListType,
  IntegerType,
  StringType,
  NoneType,
  BooleanType, Argument,
} from '../src/typings/types.ts';
import { QuarkTypes } from './typings/types.ts';

export interface QuarkDefinition {
  name: string,
}

export interface QuarkFunction extends QuarkDefinition {
  name: string,
  body: Function,
  args?: Argument[],
}

export interface QuarkVariable extends QuarkDefinition {
  name: string,
  value: ValueElement,
}

export function quarkify(fn: (...data: any[]) => any, ...args: any[]): any {
  return setValueByType(fn.call(fn, ...getValue(args)));
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

  public static object(obj: any) {
    let root: any = [];
    for (const parameter of Object.entries(obj))
      root.push(QuarkType.list(setValueByType(parameter) as ValueElement[]));
    return QuarkType.list(root);
  }
}

export function setValue(data: any): ValueElement | ValueElement[] {
  let result: ValueElement[] = [];
  if (!Array.isArray(data)) return setValueByType(data);
  for (const el of data) {
    if (Array.isArray(el)) {
      result.push(QuarkType.list(setValue(el) as ValueElement[]));
    } else {
      result.push(setValueByType(el) as ValueElement);
    }
  }
  return result;
}

function setValueByType(value: any): ValueElement | ValueElement[] {
  if (typeof value === 'string') {
    return QuarkType.string(value);
  } else if (typeof value === 'number') {
    return QuarkType.number(value);
  } else if (typeof value === 'boolean') {
    return QuarkType.boolean(value);
  } else if (Array.isArray(value)) {
    return setValue(value);
  } else if (typeof value === 'object') {
    return QuarkType.object(value);
  } else return QuarkType.none();
}

export function getValue(values: ValueElement[]): any {
  let result: any = [];
  for (const value of values) {
    if (typeof value !== 'object') result.push(value);
    else if (value.type === Types.List) {
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
          args: definition.args || [],
          js: true,
          stack: { variables: Frame.stack.map((acc) => acc.variables).flat() },
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
