const parseFile = content => {
  const lines = content.split(/\r?\n/);
  const bytecode = [];
  for (const line of lines) {
    const [op, arg] = line.split(' ');
    if (op === '') continue;
    if (arg && arg.startsWith('"') && arg.endsWith('"')) {
      bytecode.push([op, arg.slice(1, -1)]);
    } else if (arg && !isNaN(parseInt(arg))) {
      bytecode.push([op, parseInt(arg)]);
    } else {
      bytecode.push(arg ? [op, arg] : [op]);
    } 
  }
  return bytecode;
}

const fs = require('fs');

Array.prototype.peek = function(i = 1) {
  return this[this.length - i];
}

class Symbols {
  static symbols = [];

  static newFrame() {
    this.symbols.push([]);
  }

  static dropFrame() {
    this.symbols.pop();
  }

  static get frame() {
    return this.symbols.peek();
  }

  static get global() {
    return this.symbols[0];
  }
  
  static add(variable) {
    this.frame.push(variable);
  }

  static pop(variable) {
    return this.frame.splice(this.frame.findIndex(v => v[0] === variable), 1);
  }

  static get(variable) {
    const f = this.frame.find(v => v[0] === variable);
    if (f) return f;
    return this.global.find(v => v[0] === variable);
  }
}

const run = bytecode => {
  const stack = [];
  Symbols.newFrame();
  for (let i = 0; bytecode[i] !== 'HALT'; i++) {
    const [op, arg] = bytecode[i];
    switch (op) {
      case 'MAKE_LAMBDA':
        stack.push([i, Symbols.frame]);
        i += arg;
        break;
      case 'STORE':
        const v = stack.pop();
        Symbols.add([arg, v]);
        break;
      
      case 'LOAD':
        stack.push(Symbols.get(arg)[1]);
        break;
      
      case 'CALL':
        const args = replicate(() => stack.pop(), arg);
        const [offset, frame] = stack.pop();
        Symbols.newFrame()
        Symbols.add(['#return', i]);
        frame.map(x => Symbols.add(x));
        stack.push(...args);
        i = offset;
        break;

      case 'RETURN':
        const ret = Symbols.get('#return');
        Symbols.dropFrame();
        i = ret[1];
        break;
      
      case 'JUMP_ELSE':
        if (stack.pop() === 0) {
          i += arg;
          continue;
        }
        break;
      
      case 'JUMP_REL':
        i += arg;
        continue;
      
      case 'PUSH':
        stack.push(arg);
        break;
      
      case 'SUB':
        const a = stack.pop();
        const b = stack.pop();
        stack.push(b - a);
        break;

      case 'ADD':
        stack.push(stack.pop() + stack.pop());
        break;

      case 'MUL':
        stack.push(stack.pop() * stack.pop());
        break;
      
      case 'EXTERN':
        switch (arg) {
          case 0: // print
            console.log(stack.pop());
            break;
          case 1: // equality
            stack.push(stack.pop() === stack.pop() ? 1 : 0);
            break;
        }
        break;
      
      case 'DROP': 
        Symbols.pop(arg);
        break;
      case 'HALT': return;
      default: throw new Error(`Unknown opcode: ${op}`);
    }
  }
}

const replicate = (f, n) => {
  const res = [];
  for (let i = 0; i < n; i++) {
    res.push(f());
  }
  return res;
}
const content = parseFile(fs.readFileSync('bytecode.bin', 'utf8'));
const d = Date.now();
run(content);
console.log(Date.now() - d);

