import { File } from './utils/file.ts';
import { getCircularReplacer } from './utils/json.ts';
import { Interpreter } from './core/interpreter.ts';

async function main(): Promise<void> {
  // Getting sample code content and interpreting it
  const script: string = await File.read('sample/index.qrk');
  const ast = Interpreter.run(script);

  console.log(JSON.stringify(ast, getCircularReplacer(), 2))
}

await main();
