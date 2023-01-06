import {parse} from "@typescript-eslint/parser"
import {analyze, ScopeManager} from "@typescript-eslint/scope-manager";
import type {TSESTree as AST} from '@typescript-eslint/types';
import {AST_NODE_TYPES} from "@typescript-eslint/types";

function simpleParser(code: string): AST.Node {
    return parse(code, {
        ecmaVersion: 2020,
        sourceType: "script",
        range: true,
    })
}

function simpleScope(ast: AST.Node): ScopeManager {
    return analyze(ast, {
        ecmaVersion: 2020,
        sourceType: 'script',
    });
}

function runNode (node: AST.Node) {
    switch (node.type) {
        case AST_NODE_TYPES.VariableDeclaration:
            return parseVariableDeclaration(node);
        case AST_NODE_TYPES.FunctionDeclaration:
            return parseFunctionDeclaration(node);
    }
}
function parseVariableDeclaration (node: AST.VariableDeclaration) {
}

function parseVariableDeclarator (node: AST.VariableDeclarator) {
    let id = node.id;
    if (node.hasOwnProperty('typeAnnotation')) {
    }
}
function parseFunctionDeclaration(node: AST.FunctionDeclaration) {
}


export {simpleParser, simpleScope, generateConstraints}