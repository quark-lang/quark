import { Wrapper } from '../../wrapper/wrapper.ts';
import { Parser } from './parser.ts';
import { Block, Element } from '../typings/block.ts';
import { isContainer, isValue } from '../utils/runner.ts';
import { Types } from '../typings/types.ts';

type AST = Block | Element;
type Instruction = [Element, ...Block];

export class Compiler {
  private static ast: AST;

  private static value(val: Element) {
    switch (val.type) {
      case 'String':
        return '"' + val.value + '"';
      case 'Number':
        return Number(val.value);
    }
  }

  private static process(block: AST) {
    if (isValue(block)) return Compiler.value(<Element>block);
    if (isContainer(block)) {
      for (const instr of block as []) {
        this.process(instr);
      }
    } else {
      const [expr, ...args]: Instruction = <any>block;
      switch (expr.value) {
        case 'print':
          const format = (arg: Element) => {
            switch (arg.type) {
              case 'String':
                return '%s';
              case 'Number':
                return '%d';
            }
          }
          const parsedArguments = args.map(this.process);
          const parsedFormat = (<Element[]>args).map(format);
          Wrapper.Function.call('printf', [
            { type: 'char*', value: this.value({
                type: 'String',
                value: parsedFormat.join(' ') + '\\n',
              }) },
            ...parsedArguments.map((arg) => ({
              type: 'void*',
              value: arg,
            })),
          ])
          break;
      }
    }
  }

  public static compile(code: string): string {
    this.ast = Parser.parse(code);
    Wrapper.include('stdio.h');
    Wrapper.Function.define('main', 'int', [], () => {
      this.process(this.ast);
      Wrapper.Function.ret({ type: 'int', value: 0 })
    });
    Wrapper.print();
    return '';
  }
}

Compiler.compile('(print "Hello world" 56)');