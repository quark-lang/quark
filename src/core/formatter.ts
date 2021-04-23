export class Formatter {
  public static format(source: string): string {
    let formattedOutput: string[] = [];
    let state: string = '';
    source = source
      .replace("\\x1b", "\x1b")
      .replace("\\n", "\n")
      .replace("\\r", "\r")
      .replace("\\t", "\t")
      .replace("\\0", "\0")
      .replace("\\\\", "\\");
    const split = source.split(/\r?\n/g);
    for (const index in split) {
      const line = split[index];
      let formattedLine: string = '';
      if (state === 'STRING')
        formattedLine += '\\n';
      for (const char of line) {
        if (state === 'COMMENT') continue;
        if (char === '"') {
          if (state === 'STRING') state = '';
          else {
            state = 'STRING';
          }
        } else if (char === '#' && state === '') {
          state = 'COMMENT';
          continue;
        }
        formattedLine += char;
      }
      formattedOutput.push(formattedLine);
      if (state !== 'STRING') state = '';
    }
    return formattedOutput
      .join('');
  }
}