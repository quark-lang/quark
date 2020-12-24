// Tokens list
export enum Tokens {
  Node = 'Node',
  String = 'String',
  Word = 'Word',
}

// Token Node type
export type Node = '(' | ')' | '{' | '}';

// Token interface
export interface Token {
  token: Tokens,
  value: Node | string,
}
