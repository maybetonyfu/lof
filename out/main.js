import { parse } from "@typescript-eslint/parser";
import { analyze } from "@typescript-eslint/scope-manager";
function simpleParser(code) {
    return parse(code, {
        ecmaVersion: 2020,
        sourceType: "script",
        range: true,
    });
}
function simpleScope(ast) {
    return analyze(ast, {
        ecmaVersion: 2020,
        sourceType: 'script',
    });
}
let program = `
let x:number = 3
let y = x
`;
let ast = simpleParser(program);
// console.log(ast)
const scope = simpleScope(ast);
console.log(scope.variables.filter(s => s['identifiers'].length > 0));
export { simpleParser, simpleScope };
