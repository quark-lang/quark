export const module = [
  {
    name: 'print',
    func: console.log
  },
  {
    name: 'input',
    func: async (question = '') => {
      const buf = new Uint8Array(1024);
      await Deno.stdout.write(new TextEncoder().encode(question));

      const input = await Deno.stdin.read(buf);
      const answer = new TextDecoder().decode(buf.subarray(0, input));
      return answer.trim();
    }
  }
]