export interface StackValue {
  name: string,
  bound: string,
}

export interface StackSymbol {
  value: Array<string>,
  type: string,
}

export interface Stack {
  symbols: object,
  values: object,
}