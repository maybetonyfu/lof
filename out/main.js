import { parse } from "@typescript-eslint/parser";
import { analyze } from "@typescript-eslint/scope-manager";
import { open } from 'node:fs/promises';
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
let args = [...process.argv.slice(2)];
let filename;
if (args.length === 0) {
    filename = "example/test.ts";
}
else {
    filename = args[0];
}
let fileHandle = await open(filename, 'r');
let fileContent = await fileHandle.readFile({ encoding: "utf-8" });
fileHandle?.close();
let ast = simpleParser(fileContent);
let scope = simpleScope(ast);
let variables = scope.variables;
let shorthand = variables.map(v => ({
    id: v.$id,
    name: v.name,
    defs: v.defs.map(d => d.name.range),
    refs: v.references.map(r => r.identifier.range)
}));
let result = {
    ast: ast,
    scope: shorthand
};
process.stdout.write(JSON.stringify(result));
export { simpleParser, simpleScope };
