import { Interpreter } from '../src/core/interpreter.ts';
import { QuarkTypes } from './typings/types.ts';
import { QuarkCallback } from './typings/callback.ts';

export interface QuarkDefinition {
  name: string,
}

export interface QuarkFunction extends QuarkDefinition {
  name: string,
  body: Function,
}

export interface QuarkVariable extends QuarkDefinition {
  name: string,
  value: any,
}

export class QuarkModule {
  public static stack = Interpreter.stack;
  public static cwd = Interpreter.cwd;
  public static declare(namespace: string | null, type: QuarkTypes, definition: QuarkVariable | QuarkFunction): QuarkDefinition {
    const ns: string = namespace ? `${namespace}:${definition.name}` : definition.name;
    if (type === QuarkTypes.QuarkFunction) {
      definition = <QuarkFunction>definition;
      Interpreter.stack[ns] = {
        type: 'Function',
        js: true,
        func: definition.body,
      };
    } else if (type === QuarkTypes.QuarkVariable) {
      definition = <QuarkVariable>definition;
      Interpreter.stack[ns] = definition.value;
    }
    return definition;
  }

  public static async createCall(fn: QuarkCallback, ...args: any[]) {
    return await Interpreter.callFunction(args, fn);
  }
}
