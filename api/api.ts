import { Interpreter, Frame, Function, Types, Variable, ValueElement } from '../src/core/interpreter.ts';
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
