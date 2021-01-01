export type Block = (Element | Block)[];

export type ElementTypes =
  | 'String'
  | 'Number'
  | 'Word'
  | 'Node';

export interface Element {
  type: ElementTypes,
  value: string | number,
}