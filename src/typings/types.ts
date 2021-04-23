import { Block, Element } from './block';
import { FunctionFrame, Stack } from '../core/interpreter';

export enum Types {
  String = 'String',
  Integer = 'Integer',
  Function = 'Function',
  Boolean = 'Boolean',
  None = 'None',
  List = 'List',
}

export interface ListType {
  type: Types.List,
  value: ValueElement[],
}

export interface StringType {
  type: Types.String,
  value: string,
}

export interface NoneType {
  type: Types.None,
  value: undefined,
}

export interface IntegerType {
  type: Types.Integer,
  value: number,
}

export interface FunctionType {
  type: Types.Function,
  args: any[],
  name: string,
  closure: FunctionFrame,
  js: boolean,
  module?: boolean,
  body: Block | (() => {}),
}

export interface BooleanType {
  type: Types.Boolean,
  value: boolean,
}

export interface Argument extends Element {
  variadic?: boolean,
  reference?: boolean,
  block?: boolean
}

export type ValueElement =
  | StringType
  | IntegerType
  | FunctionType
  | BooleanType
  | NoneType
  | ListType;