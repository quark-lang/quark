import {
  Token,
  Tokens,
  Node,
} from '../typings/token.ts';
import { Formatter } from './formatter.ts';
export class Lexer {
  private static code: string;
  private static commentState: number = 0;

  private static lexing(): Token[] {
    let state: string = '';
    // Container variable contains processed tokens
    const container: Token[] = [];
    // Tmp variable contains temporary code chars that has been collected by tokenizer and which will be pushed to container
    const tmp: string[] = [];

    for (const char of this.code) {
      if (['(', ')', '{', '}'].includes(char) && state !== Tokens.String) {
        // Rechecking if tmp variable isn't empty before processing Node char
        if (tmp.length > 0) {
          state = '';
          container.push({ token: Tokens.Word, value: tmp.join('').trim() });
          tmp.splice(0, tmp.length);
        }
        container.push({ token: Tokens.Node, value: char as Node, });
      } else if (char === '"') {
        tmp.push(char);
        if (state === Tokens.String) {
          state = '';
          container.push({ token: Tokens.String, value: tmp.join('').trim() });
          tmp.splice(0, tmp.length);
        } else {
          state = Tokens.String;
        }
      } else if (char === ' ' && tmp.length > 0) {
        if (state === Tokens.String) {
          tmp.push(char);
        } else {
          state = '';
          container.push({ token: Tokens.Word, value: tmp.join('').trim() });
          tmp.splice(0, tmp.length);
        }
      } else {
        if (char === '/' && this.commentState === 0) this.commentState++;
        else if (char === '*' && this.commentState === 1) this.commentState++;
        else if (char === '*' && this.commentState === 2) this.commentState--;
        else if (char === '/' && this.commentState === 1) this.commentState--;
        else if (this.commentState === 0) tmp.push(char);
      }
    }
    // Removing empty tokens from container
    return container.filter((token: Token) => token.value.length > 0);
  }

  public static tokenize(source: string): Token[] {
    // Formatting content
    this.code = Formatter.format(source);
    return this.lexing();
  }
}
