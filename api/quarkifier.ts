import { getValue } from '../src/core/interpreter.ts';
import { BooleanType, IntegerType, ListType, NoneType, StringType, Types, ValueElement } from '../src/typings/types.ts';

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