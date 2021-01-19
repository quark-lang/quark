import {File} from './utils/file.ts';
import { Interpreter } from './core/interpreter.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';
import '../std/mod.ts'; // Importing Typescript STD by default.
import '../std/quark.ts';

export function arrayToObject(array: string[][]): Record<string, string> {
  let result: Record<string, string> = {};
  for (const item of array) result[item[0]] = item[1];
  return result;
}

export async function parseConfiguration(file: string): Promise<Record<string, string>> {
  try {
    const content: string = await File.read(file);
    const lines = content.split(/\r?\n/g);
    const parsed = lines.map((acc) => acc.split('=').map((x) => x.trim()));

    return arrayToObject(parsed);
  } catch (exception) {
    return {};
  }
}

export async function getQuarkFolder(): Promise<string> {
  const configuration = await parseConfiguration('.quarkrc');
  const condition = configuration['name'] === 'quark-lang'
    && Boolean(configuration['core']) === true;

  return !condition
    ? <string>Deno.env.get('QUARK')
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