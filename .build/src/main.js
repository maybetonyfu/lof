"use strict";
var __defProp = Object.defineProperty;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __hasOwnProp = Object.prototype.hasOwnProperty;
var __export = (target, all) => {
  for (var name in all)
    __defProp(target, name, { get: all[name], enumerable: true });
};
var __copyProps = (to, from, except, desc) => {
  if (from && typeof from === "object" || typeof from === "function") {
    for (let key of __getOwnPropNames(from))
      if (!__hasOwnProp.call(to, key) && key !== except)
        __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
  }
  return to;
};
var __toCommonJS = (mod) => __copyProps(__defProp({}, "__esModule", { value: true }), mod);
var main_exports = {};
__export(main_exports, {
  generateConstraints: () => generateConstraints,
  simpleParser: () => simpleParser,
  simpleScope: () => simpleScope
});
module.exports = __toCommonJS(main_exports);
var import_parser = require("@typescript-eslint/parser");
var import_scope_manager = require("@typescript-eslint/scope-manager");
var import_types = require("@typescript-eslint/types");
function simpleParser(code) {
  return (0, import_parser.parse)(code, {
    ecmaVersion: 2020,
    sourceType: "script",
    range: true
  });
}
function simpleScope(ast) {
  return (0, import_scope_manager.analyze)(ast, {
    ecmaVersion: 2020,
    sourceType: "script"
  });
}
function runNode(node) {
  switch (node.type) {
    case import_types.AST_NODE_TYPES.VariableDeclaration:
      return parseVariableDeclaration(node);
    case import_types.AST_NODE_TYPES.FunctionDeclaration:
      return parseFunctionDeclaration(node);
  }
}
function parseVariableDeclaration(node) {
}
function parseVariableDeclarator(node) {
  let id = node.id;
  if (node.hasOwnProperty("typeAnnotation")) {
  }
}
function parseFunctionDeclaration(node) {
}
function generateConstraints() {
}
console.log("hello");
// Annotate the CommonJS export names for ESM import in node:
0 && (module.exports = {
  generateConstraints,
  simpleParser,
  simpleScope
});
//# sourceMappingURL=main.js.map
