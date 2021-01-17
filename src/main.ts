import {File} from './utils/file.ts';
import {Interpreter} from './core/interpreter.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';
import {existsSync} from 'https://deno.land/std/fs/mod.ts';
import '../std/mod.ts'; // Importing Typescript STD by default.
import '../std/quark.ts';

export async function getQuarkFolder(): Promise<string> {
  const dir = await Deno.realPath('.');
  const condition = path.basename(dir) === 'quark' || (existsSync(path.join(dir, 'cli')) && existsSync(path.join(dir, 'std')));

  const envPath: string = <string>Deno.env.get(Deno.build.os === 'windows' ? 'Path' : 'PATH');
  const delimiter: string = Deno.build.os === 'windows' ? ';' : ':';

  return !condition
    ? envPath
      .split(delimiter)
      .find((x) => x.includes('quark')) || ''
    : '';
}

async function main(): Promise<void> {
  // Getting sample code content and interpreting it
  const root: string = await getQuarkFolder();
  const src = path.join(<string>root, 'cli', 'main.qrk');
  const script: string = await File.read(src);
  await Interpreter.run(script, src);
}

await main();