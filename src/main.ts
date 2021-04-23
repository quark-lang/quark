import { File } from './utils/file';
import { Interpreter } from './core/interpreter';
import * as path from 'path';
import { existsSync, readFileSync } from 'fs';
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

export function getQuarkFolder(): string {
  const configuration = existsSync('.quarkrc')
    ? parseConfiguration(readFileSync('.quarkrc', 'utf-8'))
    : {};

  const condition =
    configuration['name'] === 'quark-lang' &&
    Boolean(configuration['core']) === true;

  const variable = process.env['QUARK'];
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
main();