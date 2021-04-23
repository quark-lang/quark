import { readFile } from 'fs/promises'
export class File {
  public static async read(path: string | string[], encoding: string = 'utf-8'): Promise<string> {
    if (Array.isArray(path)) path = path.join('');
    try {
      // Decoding file content cause it's Byte array
      return <string><unknown>(await readFile(path, {
        encoding: <any>encoding,
      }));
    } catch (exception) {
      throw exception;
    }
  }
}