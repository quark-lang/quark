import { File } from './utils/file.ts';
import { Processor } from './core/processor.ts';
import { getCircularReplacer } from './utils/json.ts';

async function main(): Promise<void> {
  // Getting sample code content and interpreting it
  const script: string = await File.read('sample/index.qrk');
  const ast = Processor.parse(script);

  console.log(JSON.stringify(ast, getCircularReplacer(), 2))
}

await main();
