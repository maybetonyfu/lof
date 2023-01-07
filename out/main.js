import { parse } from "@typescript-eslint/parser";
import { analyze } from "@typescript-eslint/scope-manager";
import { AST_NODE_TYPES } from "@typescript-eslint/types";
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
}
function parseVariableDeclarator(node) {
    let id = node.id;
    if (node.hasOwnProperty('typeAnnotation')) {
    }
}
function parseFunctionDeclaration(node) {
}
function generateConstraints() {
}
console.log("hello");
export { simpleParser, simpleScope, generateConstraints };
