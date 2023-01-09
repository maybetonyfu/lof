import { parse } from "@typescript-eslint/parser";
import { analyze } from "@typescript-eslint/scope-manager";
import { AST_NODE_TYPES } from "@typescript-eslint/types";
let system = {
    ast: null,
    variables: [],
    constraints: []
};
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
function runNode(node) {
    switch (node.type) {
        case AST_NODE_TYPES.VariableDeclaration:
            return parseVariableDeclaration(node);
        case AST_NODE_TYPES.FunctionDeclaration:
            return parseFunctionDeclaration(node);
    }
}
function parseVariableDeclaration(node) {
    node.declarations.forEach(runNode);
}
function parseVariableDeclarator(node) {
    let id = node.id;
    if (id.hasOwnProperty('typeAnnotation')) {
        //   Type annotation
    }
    if (node.init !== null) {
        runNode(node.init);
    }
}
function parseFunctionDeclaration(node) {
}
function generateConstraints() {
    return [];
}
let program = "let x:number = 3";
let ast = simpleParser(program);
let scope = simpleScope(ast);
let variables = scope.variables.filter(v => v.identifiers.length !== 0);
console.log(variables[0].defs[0]);
export { simpleParser, simpleScope, generateConstraints };
