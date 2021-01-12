import { File } from './utils/file.ts';
import { Interpreter } from './core/interpreter.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';

async function main(): Promise<void> {
  // Getting sample code content and interpreting it
  const src = path.join(path.dirname(path.fromFileUrl(import.meta.url)), '..', 'cli', 'main.qrk');
  const script: string = await File.read(src);
  await Interpreter.run(script, src);

}

await main();