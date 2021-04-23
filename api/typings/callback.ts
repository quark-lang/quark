import { Block } from '../../src/typings/block';

export interface QuarkCallback {
  type: 'Function',
  js: boolean,
  body?: Block[],
  func?: Function,
}
