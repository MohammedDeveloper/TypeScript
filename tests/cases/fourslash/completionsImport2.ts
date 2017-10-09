/// <reference path="fourslash.ts" />

// @Filename: /a/b/moduleFile.ts
////export const guitar = 10;

// @Filename: /a/b/file2.ts
/////*1*/var x: /*2*/

// @Filename: /a/b/globalFile.ts
////interface Jazz { }

// @Filename: /a/b/ambientModuleFile.ts
////declare module "windyAndWarm" {
////    export const chetAtkins = "great";
////}

// @Filename: /a/b/defaultModuleFile.ts
////export default function egyptianElla() {}

verify.applyCodeActionFromCompletion("1", {
    name: "guitar",
    description: `Import 'guitar' from "./moduleFile".`,
    newFileContent: `import { guitar } from "./moduleFile";\r
\r
var x: `,
});

verify.applyCodeActionFromCompletion("1", {
    name: "chetAtkins",
    description: `Import 'chetAtkins' from "windyAndWarm".`,
    newFileContent: `import { guitar } from "./moduleFile";\r
import { chetAtkins } from "windyAndWarm";\r
\r
var x: `,
});

verify.applyCodeActionFromCompletion("1", {
    name: "egyptianElla",
    description: `Import 'egyptianElla' from "./defaultModuleFile".`,
    // TODO: GH#18445
    newFileContent: `import { guitar } from "./moduleFile";\r
import { chetAtkins } from "windyAndWarm";\r
import egyptianElla from "./defaultModuleFile";\r
\r
var x: `,
});


goTo.marker("2");
verify.completionListContains("Jazz");
