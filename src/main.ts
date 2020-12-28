import { File } from './utils/file.ts';
import { Parser } from './core/parser.ts';
import { getCircularReplacer } from './utils/json.ts';

async function main(): Promise<void> {
  // Getting sample code content and interpreting it
  const script: string = await File.read('sample/index.qrk');
  const ast = Parser.parse(script);

  console.log(JSON.stringify(ast, getCircularReplacer(), 2))
}

await main();
