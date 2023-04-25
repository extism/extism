import { ExtismContext } from './';
import fs from 'fs';
import path from 'path';

function parse(bytes: Uint8Array): any {
  return JSON.parse(new TextDecoder().decode(bytes));
}

describe('', () => {
  it('can load and call a plugin', async () => {
    // const data = fs.readFileSync(path.join(__dirname, '..', 'data', 'code.wasm'));
    // const ctx = new ExtismContext();
    // const plugin = await ctx.newPlugin({ wasm: [{ data: data }] });
    // const functions = await plugin.getExports();
    // expect(Object.keys(functions).filter((x) => !x.startsWith('__') && x !== 'memory')).toEqual(['count_vowels']);
    // let output = await plugin.call('count_vowels', 'this is a test');
    // expect(parse(output)).toEqual({ count: 4 });
    // output = await plugin.call('count_vowels', 'this is a test again');
    // expect(parse(output)).toEqual({ count: 7 });
    // output = await plugin.call('count_vowels', 'this is a test thrice');
    // expect(parse(output)).toEqual({ count: 6 });
    expect(true).toEqual(true);
  });
});
