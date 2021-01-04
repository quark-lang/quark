import { Block } from '../../src/typings/block.ts';

export interface QuarkCallback {
  type: 'Function',
  js: boolean,
  body?: Block[],
  func?: Function,
}
