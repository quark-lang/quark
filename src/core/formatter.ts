export class Formatter {
  public static format(source: string): string {
    let formattedOutput: string[] = [];
    for (const line of source.split(/\r?\n/g)) {
      let state: string = '';
      let formattedLine: string = '';
      for (const char of line) {
        if (state === 'COMMENT') continue;
        if (char === '"') {
          if (state === 'STRING') state = '';
          else state = 'STRING';
        } else if (char === '#' && state === '') {
          state = 'COMMENT';
          continue;
        }
        formattedLine += char;
      }
      formattedOutput.push(formattedLine);
    }
    return formattedOutput
      .map((line: string) => line.trim())
      .join('');
  }
}