"use strict";
var import_vitest = require("vitest");
var import_main = require("./main");
(0, import_vitest.describe)("Parser", () => {
  (0, import_vitest.it)("parse program", () => {
    let program = "let x:number;";
    let result = (0, import_main.simpleParser)(program);
    (0, import_vitest.expect)(result.type).toEqual("Program");
  });
  (0, import_vitest.it)("create scopes", () => {
    let program = "let x:number;";
    let result = (0, import_main.simpleScope)((0, import_main.simpleParser)(program));
    (0, import_vitest.expect)(result.variables).toBeGreaterThan(0);
  });
  (0, import_vitest.it)("generate constraint", () => {
    let constraints = (0, import_main.generateConstraints)();
    (0, import_vitest.expect)(constraints.length).toBeGreaterThan(0);
  });
});
//# sourceMappingURL=main.test.js.map
