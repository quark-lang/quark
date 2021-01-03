import { File } from './utils/file.ts';
import { getCircularReplacer } from './utils/json.ts';
import { Interpreter } from './core/interpreter.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';

async function main(): Promise<void> {
  // Getting sample code content and interpreting it
  const cwd = path.resolve();
  const script: string = await File.read(path.join(cwd, 'cli', 'main.qrk'));
  await Interpreter.run(script, cwd, 'cli');

}

await main();
