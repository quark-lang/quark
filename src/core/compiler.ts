import { Wrapper } from '../../wrapper/wrapper.ts';

Wrapper.include('stdio.h');

Wrapper.Function.define('welcome', 'void', [ { type: 'char**', name: 'username' } ], () => {
  Wrapper.Function.call('printf', [ { type: 'char*', value: '"Hello %s\\n"' }, Wrapper.List.getIndex('username', 0) ]);
});

Wrapper.Function.define('main', 'int', [], () => {
  Wrapper.Variable.define('username', 'char*', Wrapper.Value.get({
    type: 'Array',
    value: ['hello', 'word', ['eheh'], 'bruh'],
  }), [undefined]);
  Wrapper.Function.call('welcome', [
    { type: 'char*', value: 'username' },
  ]);
});

console.log(Wrapper.Value.get({
  type: 'Array',
  value: ['hello', 'word', ['eheh'], 'bruh'],
}));

Wrapper.print();