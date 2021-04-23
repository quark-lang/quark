import { getValue } from '../src/core/interpreter';
import { BooleanType, IntegerType, ListType, NoneType, StringType, Types, ValueElement } from '../src/typings/types';

export async function quarkify(fn: (...data: any[]) => any, ...args: any[]): Promise<any> {
  return setValueByType(await fn.call(fn, ...getValue(args)));
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
      value: typeof value === 'boolean' ? value : Boolean(value),
    }
  }

  public static none(): NoneType {
    return {
      type: Types.None,
      value: undefined,
    }
  }

  public static object(obj: any): any {
    const array = Object.entries(obj);
    return { type: 'List', value: array.length === 1 ? setValue(array[0]) : setValue(array) };
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