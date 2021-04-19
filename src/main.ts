import { File } from './utils/file.ts';
import { Interpreter } from './core/interpreter.ts';
import * as path from 'https://deno.land/std@0.83.0/path/mod.ts';
import { existsSync } from 'https://deno.land/std/fs/mod.ts';
import '../std/std.ts';

export function arrayToObject(array: string[][]): Record<string, string> {
  const result: Record<string, string> = {};
  for (const item of array)
    result[item[0]] = item[1];
  return result;
}

export function parseConfiguration(content: string): Record<string, string> {
  const lines = content.split(/\r?\n/g);
  const parsed = lines.map((acc) => acc
    .split('=')
    .map((x) => x.trim())
  );
  const error = parsed.find((x) => x[0].length === 0 || x[1].length === 0);
  const index = parsed.findIndex((x) => x === error);
  if (error !== undefined) {
    throw `Empty value or property at line ${index}: ${lines[index]}`;
  }
  return arrayToObject(parsed);
}

export async function getQuarkFolder(): Promise<string> {
  const configuration = existsSync('.quarkrc')
    ? parseConfiguration(await File.read('.quarkrc'))
    : {};

  const condition =
    configuration['name'] === 'quark-lang' &&
    Boolean(configuration['core']) === true;

  const variable = Deno.env.get('QUARK');
  if (variable === undefined) 
    throw `You have to export QUARK variable: export QUARK="path/to/quark"`;
  return !condition
    ? variable
    : '';
}

async function main(): Promise<void> {
  // Getting sample code content and interpreting it
  const root: string = await getQuarkFolder();
  const src = path.join(root, 'cli', 'main.qrk');
  try {
    const script: string = await File.read(src);
    await Interpreter.run(script, src);
  } catch(exception) {
    throw exception;
  }
}
await main();