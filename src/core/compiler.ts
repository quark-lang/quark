import { Wrapper } from '../../wrapper/wrapper.ts';
import { Parser } from './parser.ts';
import { Block, Element } from '../typings/block.ts';
import { isContainer, isValue } from '../utils/runner.ts';

type AST = (Block | Element)[];
type Instruction = [Element, ...AST];

export class Compiler {
  private static ast: AST;

  private static process(block: AST): Element | string[] | AST {
    if (isValue(block)) return <Element><unknown>block;
    if (isContainer(block)) {
      for (const instr of block as []) {
        this.process(instr);
      }
    } else {
      const [expr, ...args]: Instruction = <any>block;
      if (expr.type !== 'Word') {
        return <AST>block.map(
          (acc) => Compiler.process(<AST>acc)
        );
      }
      switch (expr.value) {
        case 'let':
          const processedValue = this.process(<AST>args[1]);
          Wrapper.Variable.define<any>(
            <string>(<Element>args[0]).value,
            <Element>processedValue
          );
          break;
        case 'print':
          Wrapper.Function.call('console.log', <Wrapper.Value<any>[]>this.process(args))
      }
    }
    return Wrapper.output;
  }

  public static compile(code: string): string {
    this.ast = Parser.parse(code);
    this.process(this.ast);
    const res = Wrapper.print();
    console.log();
    eval(res);
    return '';
  }
}

Compiler.compile('(let username "Thomas")(print "Hello" username)');