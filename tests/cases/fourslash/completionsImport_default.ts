/// <reference path="fourslash.ts" />

// @Filename: /a.ts
////export default function aa() {}

// @Filename: /b.ts
////export default function ab() {}
////export const zang = 0;

// @Filename: /c.ts
////export default function ac() {}

// @Filename: /z.ts
////import { zang } from "./b";
////import * as c from "./c";
////a/**/;

goTo.marker("");
verify.completionListContains("aa", "function aa(): void", "", "function", /*spanIndex*/ undefined, /*hasAction*/ true);
verify.completionListContains("ab", "function ab(): void", "", "function", /*spanIndex*/ undefined, /*hasAction*/ true);
verify.completionListContains("ac", "function ac(): void", "", "function", /*spanIndex*/ undefined, /*hasAction*/ true);

verify.applyCodeActionFromCompletion("", {
    name: "aa",
    description: `Import 'aa' from "./a".`,
    // TODO: GH#18445
    newFileContent: `import { zang } from "./b";
import * as c from "./c";
import aa from "./a";\r
a;`,
});

verify.applyCodeActionFromCompletion("", {
    name: "ab",
    description: `Add 'ab' to existing import declaration from "./b".`,
    // TODO: GH#18445
    newFileContent: `import ab, { zang } from "./b";
import * as c from "./c";
import aa from "./a";\r
a;`,
});

verify.applyCodeActionFromCompletion("", {
    name: "ac",
    description: `Add 'ac' to existing import declaration from "./c".`,
    // TODO: GH#18445
    newFileContent: `import ab, { zang } from "./b";
import ac, * as c from "./c";
import aa from "./a";\r
a;`,
});
