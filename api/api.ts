import {
  Frame,
  Function,
} from '../src/core/interpreter.ts';
import {
  Types,
  ValueElement,
  Argument,
} from '../src/typings/types.ts';
import { QuarkTypes } from './typings/types.ts';

export interface QuarkDefinition {
  name: string,
}

export interface QuarkFunction extends QuarkDefinition {
  name: string,
  body: Function,
  args?: any[],
}

export interface QuarkVariable extends QuarkDefinition {
  name: string,
  value: ValueElement,
}

export class QuarkModule {
  public static declare(namespace: string | null, type: QuarkTypes, definition: QuarkVariable | QuarkFunction): QuarkDefinition {
    const ns: string = namespace ? `${namespace}:${definition.name}` : definition.name;
    if (type === QuarkTypes.QuarkFunction) {
      definition = <QuarkFunction>definition;
      Frame.local.push({
        name: ns,
        value: {
          name: ns,
          type: Types.Function,
          args: definition.args as unknown as Argument[] || [],
          js: true,
          closure: Frame.frame,
          body: definition.body as (() => {}),
        },
      });
    } else if (type === QuarkTypes.QuarkVariable) {
      definition = <QuarkVariable>definition;
      Frame.local.push({
        name: ns,
        value: definition.value,
      })
    }
    return definition;
  }
}
