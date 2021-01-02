import { File } from './utils/file.ts';
import { getCircularReplacer } from './utils/json.ts';
import { Interpreter } from './core/interpreter.ts';

async function main(): Promise<void> {
  // Getting sample code content and interpreting it
  const cwd = import.meta.url.replace('src/main.ts', '').replace('file://', '');
  const script: string = await File.read(cwd + 'sample/index.qrk');
  await Interpreter.run(script, cwd, 'sample');

}

await main();
