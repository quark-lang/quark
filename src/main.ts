import { File } from './utils/file.ts';
import { getCircularReplacer } from './utils/json.ts';
import { Interpreter } from './core/interpreter.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';

async function main(): Promise<void> {
  // Getting sample code content and interpreting it
  const cliPath = path.join(Interpreter.parentDir(path.fromFileUrl(import.meta.url), 2), 'cli', 'main.qrk');
  const script: string = await File.read(cliPath);
  await Interpreter.run(script, cliPath);
}

await main();
