import { Interpreter } from './core/interpreter.ts';

async function input(
  question: string = '',
  stdin = Deno.stdin,
  stdout = Deno.stdout
) {
  const buf = new Uint8Array(1024);
  await stdout.write(new TextEncoder().encode(question));

  const input = <number>await stdin.read(buf);
  const answer = new TextDecoder().decode(buf.subarray(0, input));
  return answer.trim();
}

console.log('Quark v0.0.1')
while (true) {
  const code = await input('>>> ');
  console.log(Interpreter.run(code));
}