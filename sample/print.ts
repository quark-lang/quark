import { Interpreter } from '../src/core/interpreter.ts';
export const module = [
  {
    name: 'print',
    func: console.log
  },
  {
    name: 'exec',
    func: async function(code: string) {
      return await Interpreter.run(code);
    }
  },
  {
    name: 'input',
    func: async (question = '') => {
      const buf = new Uint8Array(1024);
      await Deno.stdout.write(new TextEncoder().encode(question));

      const input = await Deno.stdin.read(buf);
      const answer = new TextDecoder().decode(buf.subarray(0, input as number | undefined));
      return answer.trim();
    }
  }
]