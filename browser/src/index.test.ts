import { sum } from './';
describe('sum function', () => {
  it('adds 2 numbers', () => {
    expect(sum(1, 2)).toBe(3);
  });
});
