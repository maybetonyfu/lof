import { describe, expect, it } from 'vitest';
import { simpleParser, simpleScope, generateConstraints } from "./main";
describe('Parser', () => {
    it('parse program', () => {
        let program = "let x:number;";
        let result = simpleParser(program);
        expect(result.type).toEqual('Program');
    });
    it('create scopes', () => {
        let program = "let x:number;";
        let result = simpleScope(simpleParser(program));
        expect(result.variables).toBeGreaterThan(0);
    });
    it('generate constraint', () => {
        let constraints = generateConstraints();
        expect(constraints.length).toBeGreaterThan(0);
    });
});
