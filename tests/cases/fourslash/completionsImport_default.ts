/// <reference path="fourslash.ts" />

// @Filename: /a.ts
////export default function aa() {}

// @Filename: /b.ts
////export default function ab() {}
////export const zang = 0;

// @Filename: /c.ts
////import { zang } from "./b";
////a/**/;

goTo.marker("");
verify.completionListContains("aa", "function aa(): void", "", "function", /*spanIndex*/ undefined, /*hasAction*/ true);
verify.completionListContains("ab", "function ab(): void", "", "function", /*spanIndex*/ undefined, /*hasAction*/ true);

verify.applyCodeActionFromCompletion("", {
    name: "aa",
    description: `Import 'aa' from "./a".`,
    // TODO: GH#18445
    newFileContent: `import { zang } from "./b";
import aa from "./a";\r
a;`,
});

verify.applyCodeActionFromCompletion("", {
    name: "ab",
    description: `Add 'ab' to existing import declaration from "./b".`,
    // TODO: GH#18445
    newFileContent: `import ab, { zang } from "./b";
import aa from "./a";\r
a;`,
});
