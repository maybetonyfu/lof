import { parse } from "@typescript-eslint/parser"
import { analyze, ScopeManager } from "@typescript-eslint/scope-manager";
import type { TSESTree as AST } from '@typescript-eslint/types';
import { AST_NODE_TYPES } from "@typescript-eslint/types";
import {Variable} from "@typescript-eslint/scope-manager/dist/variable";



interface System {
  ast: AST.Node | null,
  variables: Variable[],
  constraints: any[]
}

let system : System = {
  ast: null,
  variables: [],
  constraints: []
}

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

function runNode(node: AST.Node) {
  switch (node.type) {
    case AST_NODE_TYPES.VariableDeclaration:
      return parseVariableDeclaration(node);
    case AST_NODE_TYPES.FunctionDeclaration:
      return parseFunctionDeclaration(node);
  }
}

function parseVariableDeclaration(node: AST.VariableDeclaration) {
  node.declarations.forEach(runNode)
}

function parseVariableDeclarator(node: AST.VariableDeclarator) {
  let id = node.id;
  if (id.hasOwnProperty('typeAnnotation')) {
  //   Type annotation
  }

  if (node.init !== null) {
    runNode(node.init)
  }

}
function parseFunctionDeclaration(node: AST.FunctionDeclaration) {

}

function generateConstraints() {
 return []
}


let program = "let x:number = 3"
let ast = simpleParser(program)
let scope = simpleScope(ast)

let variables = scope.variables.filter(v => v.identifiers.length !== 0)
console.log(variables[0].defs[0])
export { simpleParser, simpleScope, generateConstraints }