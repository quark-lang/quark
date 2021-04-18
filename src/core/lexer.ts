import {
  Token,
  Tokens,
} from '../typings/token.ts';
import type { Node } from '../typings/token.ts';
import { Formatter } from './formatter.ts';
export class Lexer {
  private static code: string;
  private static commentState: number = 0;

  private static lexing(): Token[] {
    let state: string = '';
    let escaped: boolean = false;
    // Container variable contains processed tokens
    const container: Token[] = [];
    // Tmp variable contains temporary code chars that has been collected by tokenizer and which will be pushed to container
    const tmp: string[] = [];
    this.commentState = 0;
    for (const index in this.code.split('')) {
      const char = this.code[index];
      if (['(', ')', '{', '}', '[', ']'].includes(char) && state !== Tokens.String) {
        // Rechecking if tmp variable isn't empty before processing Node char
        if (tmp.length > 0) {
          state = '';
          container.push({ token: Tokens.Word, value: tmp.join('').trim() });
          tmp.splice(0, tmp.length);
        }
        container.push({ token: Tokens.Node, value: char as Node, });
      } else if (char === '"' && state !== 'COMMENT' && !escaped) {
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
        } else if (state !== 'COMMENT') {
          state = '';
          container.push({ token: Tokens.Word, value: tmp.join('').trim() });
          tmp.splice(0, tmp.length);
        }
      } else {
        if (state !== Tokens.String) {
          if (char === '/') {
            if (this.code[Number(index) + 1] === '*') {
              this.commentState = 1;
              state = 'COMMENT';
              continue;
            } else if (this.commentState === -1 && state === 'COMMENT') {
              this.commentState = 0;
              state = '';
              continue;
            }
          } else if (char === '*' && state === 'COMMENT') {
            if (this.code[Number(index) + 1] === '/' && this.commentState === 1) {
              this.commentState = -1;
            }
            continue;
          }
        }
        if (this.commentState === 0) {
          if (char === '\\') {
            switch (this.code[Number(index) + 1]) {
              case 'n':
                tmp.push('\n');
                break;
              case 'r':
                tmp.push('\r');
                break;
              case 't':
                tmp.push('\t');
                break;
              case 'b':
                tmp.push('\b');
                break;
              case 'f':
                tmp.push('\f');
                break;
              case 'v':
                tmp.push('\v');
                break;
              default:
                tmp.push(this.code[Number(index) + 1]);
            }
            escaped = true;
          }
          else if (!escaped) {
            tmp.push(char);
          } else escaped = false;
        }
      }
    }
    // Removing empty tokens from container
    return container.filter((token: Token) => token.value.length > 0);
  }

  public static tokenize(source: string): Token[] {
    // Formatting content
    this.code = Formatter.format(source);
    this.commentState = 0;
    return this.lexing();
  }
}
