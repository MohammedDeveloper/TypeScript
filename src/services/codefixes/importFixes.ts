/* @internal */
namespace ts.codefix {
    registerCodeFix({
        errorCodes: [
            Diagnostics.Cannot_find_name_0.code,
            Diagnostics.Cannot_find_name_0_Did_you_mean_1.code,
            Diagnostics.Cannot_find_namespace_0.code,
            Diagnostics._0_refers_to_a_UMD_global_but_the_current_file_is_a_module_Consider_adding_an_import_instead.code
        ],
        getCodeActions: getImportCodeActions
    });

    type ImportCodeActionKind = "CodeChange" | "InsertingIntoExistingImport" | "NewImport";
    // this is a module id -> module import declaration map
    type ImportDeclarationMap = AnyImportSyntax[][];

    interface ImportCodeAction extends CodeAction {
        kind: ImportCodeActionKind;
        moduleSpecifier?: string;
    }

    export interface ImportCodeFixContext extends TextChangesContext {
        host: LanguageServiceHost;
        symbolName: string;
        sourceFile: SourceFile;
        checker: TypeChecker;
        compilerOptions: CompilerOptions;
        getCanonicalFileName: (fileName: string) => string;
        // this is a module id -> module import declaration map
        cachedImportDeclarations?: ImportDeclarationMap;
        symbolToken?: Node;
    }

    enum ModuleSpecifierComparison {
        Better,
        Equal,
        Worse
    }

    class ImportCodeActionMap {
        private symbolIdToActionMap: ImportCodeAction[][] = [];

        addAction(symbolId: number, newAction: ImportCodeAction) {
            if (!newAction) {
                return;
            }

            const actions = this.symbolIdToActionMap[symbolId];
            if (!actions) {
                this.symbolIdToActionMap[symbolId] = [newAction];
                return;
            }

            if (newAction.kind === "CodeChange") {
                actions.push(newAction);
                return;
            }

            const updatedNewImports: ImportCodeAction[] = [];
            for (const existingAction of this.symbolIdToActionMap[symbolId]) {
                if (existingAction.kind === "CodeChange") {
                    // only import actions should compare
                    updatedNewImports.push(existingAction);
                    continue;
                }

                switch (this.compareModuleSpecifiers(existingAction.moduleSpecifier, newAction.moduleSpecifier)) {
                    case ModuleSpecifierComparison.Better:
                        // the new one is not worth considering if it is a new import.
                        // However if it is instead a insertion into existing import, the user might want to use
                        // the module specifier even it is worse by our standards. So keep it.
                        if (newAction.kind === "NewImport") {
                            return;
                        }
                        // falls through
                    case ModuleSpecifierComparison.Equal:
                        // the current one is safe. But it is still possible that the new one is worse
                        // than another existing one. For example, you may have new imports from "./foo/bar"
                        // and "bar", when the new one is "bar/bar2" and the current one is "./foo/bar". The new
                        // one and the current one are not comparable (one relative path and one absolute path),
                        // but the new one is worse than the other one, so should not add to the list.
                        updatedNewImports.push(existingAction);
                        break;
                    case ModuleSpecifierComparison.Worse:
                        // the existing one is worse, remove from the list.
                        continue;
                }
            }
            // if we reach here, it means the new one is better or equal to all of the existing ones.
            updatedNewImports.push(newAction);
            this.symbolIdToActionMap[symbolId] = updatedNewImports;
        }

        addActions(symbolId: number, newActions: ImportCodeAction[]) {
            for (const newAction of newActions) {
                this.addAction(symbolId, newAction);
            }
        }

        getAllActions() {
            let result: ImportCodeAction[] = [];
            for (const key in this.symbolIdToActionMap) {
                result = concatenate(result, this.symbolIdToActionMap[key]);
            }
            return result;
        }

        private compareModuleSpecifiers(moduleSpecifier1: string, moduleSpecifier2: string): ModuleSpecifierComparison {
            if (moduleSpecifier1 === moduleSpecifier2) {
                return ModuleSpecifierComparison.Equal;
            }

            // if moduleSpecifier1 (ms1) is a substring of ms2, then it is better
            if (moduleSpecifier2.indexOf(moduleSpecifier1) === 0) {
                return ModuleSpecifierComparison.Better;
            }

            if (moduleSpecifier1.indexOf(moduleSpecifier2) === 0) {
                return ModuleSpecifierComparison.Worse;
            }

            // if both are relative paths, and ms1 has fewer levels, then it is better
            if (isExternalModuleNameRelative(moduleSpecifier1) && isExternalModuleNameRelative(moduleSpecifier2)) {
                const regex = new RegExp(directorySeparator, "g");
                const moduleSpecifier1LevelCount = (moduleSpecifier1.match(regex) || []).length;
                const moduleSpecifier2LevelCount = (moduleSpecifier2.match(regex) || []).length;

                return moduleSpecifier1LevelCount < moduleSpecifier2LevelCount
                    ? ModuleSpecifierComparison.Better
                    : moduleSpecifier1LevelCount === moduleSpecifier2LevelCount
                        ? ModuleSpecifierComparison.Equal
                        : ModuleSpecifierComparison.Worse;
            }

            // the equal cases include when the two specifiers are not comparable.
            return ModuleSpecifierComparison.Equal;
        }
    }

    function createCodeAction(
        description: DiagnosticMessage,
        diagnosticArgs: string[],
        changes: FileTextChanges[],
        kind: ImportCodeActionKind,
        moduleSpecifier?: string): ImportCodeAction {
        return {
            description: formatMessage.apply(undefined, [undefined, description].concat(<any[]>diagnosticArgs)),
            changes,
            kind,
            moduleSpecifier
        };
    }

    function convertToImportCodeFixContext(context: CodeFixContext): ImportCodeFixContext {
        const useCaseSensitiveFileNames = context.host.useCaseSensitiveFileNames ? context.host.useCaseSensitiveFileNames() : false;
        const checker = context.program.getTypeChecker();
        const token = getTokenAtPosition(context.sourceFile, context.span.start, /*includeJsDocComment*/ false);
        return {
            host: context.host,
            newLineCharacter: context.newLineCharacter,
            rulesProvider: context.rulesProvider,
            sourceFile: context.sourceFile,
            checker,
            compilerOptions: context.program.getCompilerOptions(),
            cachedImportDeclarations: [],
            getCanonicalFileName: createGetCanonicalFileName(useCaseSensitiveFileNames),
            symbolName: token.getText(),
            symbolToken: token
        };
    }

    export const enum ImportKind {
        Named,
        Default,
        Namespace,
    }

    export function getCodeActionForImport(moduleSymbol: Symbol, context: ImportCodeFixContext, symbolName: string, kind: ImportKind): ImportCodeAction[] {
        Debug.assert(context.symbolName === symbolName); //If true, don't pass as separate parameter.
        const existingDeclarations = getImportDeclarations(moduleSymbol, context.checker, context.sourceFile, context.cachedImportDeclarations);
        if (existingDeclarations.length > 0) {
            // With an existing import statement, there are more than one actions the user can do.
            return getCodeActionsForExistingImport(moduleSymbol, context, symbolName, kind, existingDeclarations);
        }
        else {
            return [getCodeActionForNewImport(context, moduleSymbol, symbolName, kind, /*existingModuleSpecifier*/ undefined)];
        }
    }

    function getImportDeclarations(moduleSymbol: Symbol, checker: TypeChecker, { imports }: SourceFile, cachedImportDeclarations: ImportDeclarationMap = []): ReadonlyArray<AnyImportSyntax> {
        const moduleSymbolId = getUniqueSymbolId(moduleSymbol, checker);

        const cached = cachedImportDeclarations[moduleSymbolId];
        if (cached) {
            return cached;
        }

        const existingDeclarations = mapDefined(imports, importModuleSpecifier =>
            checker.getSymbolAtLocation(importModuleSpecifier) === moduleSymbol ? getImportDeclaration(importModuleSpecifier) : undefined);
        cachedImportDeclarations[moduleSymbolId] = existingDeclarations;
        return existingDeclarations;

        function getImportDeclaration({ parent }: LiteralExpression): AnyImportSyntax | undefined {
            switch (parent.kind) {
                case SyntaxKind.ImportDeclaration:
                    return parent as ImportDeclaration;
                case SyntaxKind.ExternalModuleReference:
                    return (parent as ExternalModuleReference).parent;
                default:
                    Debug.assert(parent.kind === SyntaxKind.ExportDeclaration);
                    // Ignore these, can't add imports to them.
                    return undefined;
            }
        }
    }

    function createChangeTracker(context: TextChangesContext): textChanges.ChangeTracker {
        return textChanges.ChangeTracker.fromContext(context);
    }

    function getCodeActionForNewImport(
        context: ImportCodeFixContext,
        moduleSymbol: Symbol,
        symbolName: string,
        kind: ImportKind,
        moduleSpecifier: string | undefined,
    ): ImportCodeAction {
        const { sourceFile, getCanonicalFileName, newLineCharacter, host, compilerOptions } = context;
        let lastImportDeclaration: Node;

        if (!lastImportDeclaration) {
            // insert after any existing imports
            //TODO: pass this in as a parameter?
            for (let i = sourceFile.statements.length - 1; i >= 0; i--) {
                const statement = sourceFile.statements[i];
                if (statement.kind === SyntaxKind.ImportEqualsDeclaration || statement.kind === SyntaxKind.ImportDeclaration) {
                    lastImportDeclaration = statement;
                    break;
                }
            }
        }

        const moduleSpecifierWithoutQuotes = stripQuotes(moduleSpecifier || getModuleSpecifierForNewImport(sourceFile, moduleSymbol, compilerOptions, getCanonicalFileName, host));
        const changeTracker = createChangeTracker(context);
        //TODO:neater
        const importClause = (() => {
            switch (kind) {
                case ImportKind.Default:
                    return createImportClause(createIdentifier(symbolName), /*namedBindings*/ undefined)
                case ImportKind.Namespace:
                    return createImportClause(/*name*/ undefined, createNamespaceImport(createIdentifier(symbolName)));
                case ImportKind.Named:
                    return createImportClause(/*name*/ undefined, createNamedImports([createImportSpecifier(/*propertyName*/ undefined, createIdentifier(symbolName))]));
                default:
                    Debug.fail();
            }
        })();
        const moduleSpecifierLiteral = createLiteral(moduleSpecifierWithoutQuotes);
        moduleSpecifierLiteral.singleQuote = getSingleQuoteStyleFromExistingImports(sourceFile);
        const importDecl = createImportDeclaration(/*decorators*/ undefined, /*modifiers*/ undefined, importClause, moduleSpecifierLiteral);
        if (!lastImportDeclaration) {
            changeTracker.insertNodeAt(sourceFile, getSourceFileImportLocation(sourceFile), importDecl, { suffix: `${context.newLineCharacter}${context.newLineCharacter}` });
        }
        else {
            changeTracker.insertNodeAfter(sourceFile, lastImportDeclaration, importDecl, { suffix: newLineCharacter });
        }

        // if this file doesn't have any import statements, insert an import statement and then insert a new line
        // between the only import statement and user code. Otherwise just insert the statement because chances
        // are there are already a new line seperating code and import statements.
        return createCodeAction(
            Diagnostics.Import_0_from_1,
            [symbolName, moduleSpecifierWithoutQuotes],
            changeTracker.getChanges(),
            "NewImport",
            moduleSpecifierWithoutQuotes
        );
    }

    function getSourceFileImportLocation(node: SourceFile) {
        // For a source file, it is possible there are detached comments we should not skip
        const text = node.text;
        let ranges = getLeadingCommentRanges(text, 0);
        if (!ranges) return 0;
        let position = 0;
        // However we should still skip a pinned comment at the top
        if (ranges.length && ranges[0].kind === SyntaxKind.MultiLineCommentTrivia && isPinnedComment(text, ranges[0])) {
            position = ranges[0].end + 1;
            ranges = ranges.slice(1);
        }
        // As well as any triple slash references
        for (const range of ranges) {
            if (range.kind === SyntaxKind.SingleLineCommentTrivia && isRecognizedTripleSlashComment(node.text, range.pos, range.end)) {
                position = range.end + 1;
                continue;
            }
            break;
        }
        return position;
    }

    function getSingleQuoteStyleFromExistingImports(sourceFile: SourceFile) {
        const firstModuleSpecifier = forEach(sourceFile.statements, node => {
            if (isImportDeclaration(node) || isExportDeclaration(node)) {
                if (node.moduleSpecifier && isStringLiteral(node.moduleSpecifier)) {
                    return node.moduleSpecifier;
                }
            }
            else if (isImportEqualsDeclaration(node)) {
                if (isExternalModuleReference(node.moduleReference) && isStringLiteral(node.moduleReference.expression)) {
                    return node.moduleReference.expression;
                }
            }
        });
        if (firstModuleSpecifier) {
            return sourceFile.text.charCodeAt(firstModuleSpecifier.getStart()) === CharacterCodes.singleQuote;
        }
    }

    function getModuleSpecifierForNewImport(sourceFile: SourceFile, moduleSymbol: Symbol, options: CompilerOptions, getCanonicalFileName: (file: string) => string, host: LanguageServiceHost): string | undefined {
        const moduleFileName = moduleSymbol.valueDeclaration.getSourceFile().fileName;
        const sourceDirectory = getDirectoryPath(sourceFile.fileName);

        return tryGetModuleNameFromAmbientModule(moduleSymbol) ||
            tryGetModuleNameFromTypeRoots(options, host, getCanonicalFileName, moduleFileName) ||
            tryGetModuleNameAsNodeModule(options, moduleFileName, host, getCanonicalFileName, sourceDirectory) ||
            tryGetModuleNameFromBaseUrl(options, moduleFileName, getCanonicalFileName) ||
            tryGetModuleNameFromRootDirs(options, moduleFileName, sourceDirectory, getCanonicalFileName) ||
            removeFileExtension(getRelativePath(moduleFileName, sourceDirectory, getCanonicalFileName));
    }

    function tryGetModuleNameFromAmbientModule(moduleSymbol: Symbol): string | undefined {
        const decl = moduleSymbol.valueDeclaration;
        if (isModuleDeclaration(decl) && isStringLiteral(decl.name)) {
            return decl.name.text;
        }
    }

    function tryGetModuleNameFromBaseUrl(options: CompilerOptions, moduleFileName: string, getCanonicalFileName: (file: string) => string): string | undefined {
        if (!options.baseUrl) {
            return undefined;
        }

        let relativeName = getRelativePathIfInDirectory(moduleFileName, options.baseUrl, getCanonicalFileName);
        if (!relativeName) {
            return undefined;
        }

        const relativeNameWithIndex = removeFileExtension(relativeName);
        relativeName = removeExtensionAndIndexPostFix(relativeName);

        if (options.paths) {
            for (const key in options.paths) {
                for (const pattern of options.paths[key]) {
                    const indexOfStar = pattern.indexOf("*");
                    if (indexOfStar === 0 && pattern.length === 1) {
                        continue;
                    }
                    else if (indexOfStar !== -1) {
                        const prefix = pattern.substr(0, indexOfStar);
                        const suffix = pattern.substr(indexOfStar + 1);
                        if (relativeName.length >= prefix.length + suffix.length &&
                            startsWith(relativeName, prefix) &&
                            endsWith(relativeName, suffix)) {
                            const matchedStar = relativeName.substr(prefix.length, relativeName.length - suffix.length);
                            return key.replace("\*", matchedStar);
                        }
                    }
                    else if (pattern === relativeName || pattern === relativeNameWithIndex) {
                        return key;
                    }
                }
            }
        }

        return relativeName;
    }

    function tryGetModuleNameFromRootDirs(options: CompilerOptions, moduleFileName: string, sourceDirectory: string, getCanonicalFileName: (file: string) => string): string | undefined {
        if (options.rootDirs) {
            const normalizedTargetPath = getPathRelativeToRootDirs(moduleFileName, options.rootDirs, getCanonicalFileName);
            const normalizedSourcePath = getPathRelativeToRootDirs(sourceDirectory, options.rootDirs, getCanonicalFileName);
            if (normalizedTargetPath !== undefined) {
                const relativePath = normalizedSourcePath !== undefined ? getRelativePath(normalizedTargetPath, normalizedSourcePath, getCanonicalFileName) : normalizedTargetPath;
                return removeFileExtension(relativePath);
            }
        }
        return undefined;
    }

    function tryGetModuleNameFromTypeRoots(options: CompilerOptions, host: GetEffectiveTypeRootsHost, getCanonicalFileName: (file: string) => string, moduleFileName: string): string | undefined {
        const typeRoots = getEffectiveTypeRoots(options, host);
        if (!typeRoots) {
            return undefined;
        }

        const normalizedTypeRoots = map(typeRoots, typeRoot => toPath(typeRoot, /*basePath*/ undefined, getCanonicalFileName));
        for (const typeRoot of normalizedTypeRoots) {
            if (startsWith(moduleFileName, typeRoot)) {
                const relativeFileName = moduleFileName.substring(typeRoot.length + 1);
                return removeExtensionAndIndexPostFix(relativeFileName);
            }
        }
    }

    function tryGetModuleNameAsNodeModule(
        options: CompilerOptions,
        moduleFileName: string,
        host: LanguageServiceHost,
        getCanonicalFileName: (file: string) => string,
        sourceDirectory: string,
    ): string | undefined {
        if (getEmitModuleResolutionKind(options) !== ModuleResolutionKind.NodeJs) {
            // nothing to do here
            return undefined;
        }

        const parts = getNodeModulePathParts(moduleFileName);

        if (!parts) {
            return undefined;
        }

        // Simplify the full file path to something that can be resolved by Node.

        // If the module could be imported by a directory name, use that directory's name
        let moduleSpecifier = getDirectoryOrExtensionlessFileName(moduleFileName);
        // Get a path that's relative to node_modules or the importing file's path
        moduleSpecifier = getNodeResolvablePath(moduleSpecifier);
        // If the module was found in @types, get the actual Node package name
        return getPackageNameFromAtTypesDirectory(moduleSpecifier);

        function getDirectoryOrExtensionlessFileName(path: string): string {
            // If the file is the main module, it can be imported by the package name
            const packageRootPath = path.substring(0, parts.packageRootIndex);
            const packageJsonPath = combinePaths(packageRootPath, "package.json");
            if (host.fileExists(packageJsonPath)) {
                const packageJsonContent = JSON.parse(host.readFile(packageJsonPath));
                if (packageJsonContent) {
                    const mainFileRelative = packageJsonContent.typings || packageJsonContent.types || packageJsonContent.main;
                    if (mainFileRelative) {
                        const mainExportFile = toPath(mainFileRelative, packageRootPath, getCanonicalFileName);
                        if (mainExportFile === getCanonicalFileName(path)) {
                            return packageRootPath;
                        }
                    }
                }
            }

            // We still have a file name - remove the extension
            const fullModulePathWithoutExtension = removeFileExtension(path);

            // If the file is /index, it can be imported by its directory name
            if (getCanonicalFileName(fullModulePathWithoutExtension.substring(parts.fileNameIndex)) === "/index") {
                return fullModulePathWithoutExtension.substring(0, parts.fileNameIndex);
            }

            return fullModulePathWithoutExtension;
        }

        function getNodeResolvablePath(path: string): string {
            const basePath = path.substring(0, parts.topLevelNodeModulesIndex);
            if (sourceDirectory.indexOf(basePath) === 0) {
                // if node_modules folder is in this folder or any of its parent folders, no need to keep it.
                return path.substring(parts.topLevelPackageNameIndex + 1);
            }
            else {
                return getRelativePath(path, sourceDirectory, getCanonicalFileName);
            }
        }
    }

    function getNodeModulePathParts(fullPath: string) {
        // If fullPath can't be valid module file within node_modules, returns undefined.
        // Example of expected pattern: /base/path/node_modules/[@scope/otherpackage/@otherscope/node_modules/]package/[subdirectory/]file.js
        // Returns indices:                       ^            ^                                                      ^             ^

        let topLevelNodeModulesIndex = 0;
        let topLevelPackageNameIndex = 0;
        let packageRootIndex = 0;
        let fileNameIndex = 0;

        const enum States {
            BeforeNodeModules,
            NodeModules,
            Scope,
            PackageContent
        }

        let partStart = 0;
        let partEnd = 0;
        let state = States.BeforeNodeModules;

        while (partEnd >= 0) {
            partStart = partEnd;
            partEnd = fullPath.indexOf("/", partStart + 1);
            switch (state) {
                case States.BeforeNodeModules:
                    if (fullPath.indexOf("/node_modules/", partStart) === partStart) {
                        topLevelNodeModulesIndex = partStart;
                        topLevelPackageNameIndex = partEnd;
                        state = States.NodeModules;
                    }
                    break;
                case States.NodeModules:
                case States.Scope:
                    if (state === States.NodeModules && fullPath.charAt(partStart + 1) === "@") {
                        state = States.Scope;
                    }
                    else {
                        packageRootIndex = partEnd;
                        state = States.PackageContent;
                    }
                    break;
                case States.PackageContent:
                    if (fullPath.indexOf("/node_modules/", partStart) === partStart) {
                        state = States.NodeModules;
                    }
                    else {
                        state = States.PackageContent;
                    }
                    break;
            }
        }

        fileNameIndex = partStart;

        return state > States.NodeModules ? { topLevelNodeModulesIndex, topLevelPackageNameIndex, packageRootIndex, fileNameIndex } : undefined;
    }

    function getPathRelativeToRootDirs(path: string, rootDirs: string[], getCanonicalFileName: (fileName: string) => string) {
        for (const rootDir of rootDirs) {
            const relativeName = getRelativePathIfInDirectory(path, rootDir, getCanonicalFileName);
            if (relativeName !== undefined) {
                return relativeName;
            }
        }
        return undefined;
    }

    function removeExtensionAndIndexPostFix(fileName: string) {
        fileName = removeFileExtension(fileName);
        if (endsWith(fileName, "/index")) {
            fileName = fileName.substr(0, fileName.length - 6/* "/index".length */);
        }
        return fileName;
    }

    function getRelativePathIfInDirectory(path: string, directoryPath: string, getCanonicalFileName: (fileName: string) => string) {
        const relativePath = getRelativePathToDirectoryOrUrl(directoryPath, path, directoryPath, getCanonicalFileName, /*isAbsolutePathAnUrl*/ false);
        return isRootedDiskPath(relativePath) || startsWith(relativePath, "..") ? undefined : relativePath;
    }

    function getRelativePath(path: string, directoryPath: string, getCanonicalFileName: (fileName: string) => string) {
        const relativePath = getRelativePathToDirectoryOrUrl(directoryPath, path, directoryPath, getCanonicalFileName, /*isAbsolutePathAnUrl*/ false);
        return !pathIsRelative(relativePath) ? "./" + relativePath : relativePath;
    }

    function getCodeActionsForExistingImport(moduleSymbol: Symbol, context: ImportCodeFixContext, symbolName: string, kind: ImportKind, declarations: ReadonlyArray<AnyImportSyntax>): ImportCodeAction[] {
        Debug.assert(context.symbolName === symbolName); //If true, don't pass as separate parameters!
        const { symbolName: name, sourceFile, symbolToken } = context;
        const { namespaceImportDeclaration, namedImportDeclaration, existingModuleSpecifier } = getExistingImportDeclarationsInfo(declarations);

        const actions: ImportCodeAction[] = [];
        if (symbolToken && namespaceImportDeclaration) {
            actions.push(getCodeActionForNamespaceImport(namespaceImportDeclaration, name, sourceFile, symbolToken, context));
        }

        if (!isNamespaceImport && namedImportDeclaration && namedImportDeclaration.importClause &&
            (namedImportDeclaration.importClause.name || namedImportDeclaration.importClause.namedBindings)) {
            /**
             * If the existing import declaration already has a named import list, just
             * insert the identifier into that list.
             */
            //TODO: above comment out of date, must handle default imports too.
            const changeTracker = createChangeTracker(context);
            getTextChangeForImportClause(namedImportDeclaration.importClause, name, kind === ImportKind.Default, sourceFile, changeTracker); //TODO: test this with namespace import...
            const moduleSpecifierWithoutQuotes = stripQuotes(namedImportDeclaration.moduleSpecifier.getText());
            actions.push(createCodeAction(
                Diagnostics.Add_0_to_existing_import_declaration_from_1,
                [name, moduleSpecifierWithoutQuotes],
                changeTracker.getChanges(),
                "InsertingIntoExistingImport",
                moduleSpecifierWithoutQuotes
            ));
        }
        else {
            // we need to create a new import statement, but the existing module specifier can be reused.
            actions.push(getCodeActionForNewImport(context, moduleSymbol, symbolName, kind, existingModuleSpecifier));
        }
        return actions;
    }

    function getTextChangeForImportClause(importClause: ImportClause, name: string, isDefault: boolean, sourceFile: SourceFile, changeTracker: textChanges.ChangeTracker): void {
        if (isDefault) {
            if (importClause.name) {
                throw new Error("TODO"); //!
            }

            changeTracker.replaceNode(sourceFile, importClause, createImportClause(createIdentifier(name), importClause.namedBindings));
        }
        else {
            const importList = <NamedImports>importClause.namedBindings; //TODO: what makes this cast valid? make a test...
            const newImportSpecifier = createImportSpecifier(/*propertyName*/ undefined, createIdentifier(name));
            // case 1:
            // original text: import default from "module"
            // change to: import default, { name } from "module"
            // case 2:
            // original text: import {} from "module"
            // change to: import { name } from "module"
            if (!importList || importList.elements.length === 0) {
                const newImportClause = createImportClause(importClause.name, createNamedImports([newImportSpecifier]));
                changeTracker.replaceNode(sourceFile, importClause, newImportClause);
            }
            else {
                /**
                 * If the import list has one import per line, preserve that. Otherwise, insert on same line as last element
                 *     import {
                 *         foo
                 *     } from "./module";
                 */
                changeTracker.insertNodeInListAfter(
                    sourceFile,
                    importList.elements[importList.elements.length - 1],
                    newImportSpecifier);
            }
        }
    }

    function getCodeActionForNamespaceImport(declaration: ImportDeclaration | ImportEqualsDeclaration, name: string, sourceFile: SourceFile, symbolToken: Node, context: TextChangesContext): ImportCodeAction {
        const namespacePrefix = stripQuotes(declaration.kind === SyntaxKind.ImportDeclaration
            ? (<NamespaceImport>declaration.importClause.namedBindings).name.getText()
            : declaration.name.getText());

        /**
         * Cases:
         *     import * as ns from "mod"
         *     import default, * as ns from "mod"
         *     import ns = require("mod")
         *
         * Because there is no import list, we alter the reference to include the
         * namespace instead of altering the import declaration. For example, "foo" would
         * become "ns.foo"
         */
        return createCodeAction(
            Diagnostics.Change_0_to_1,
            [name, `${namespacePrefix}.${name}`],
            createChangeTracker(context).replaceNode(sourceFile, symbolToken, createPropertyAccess(createIdentifier(namespacePrefix), name)).getChanges(),
            "CodeChange"
        );
    }

    interface ExistingImportDeclarationsInfo {
        readonly namespaceImportDeclaration: AnyImportSyntax | undefined;
        readonly namedImportDeclaration: ImportDeclaration | undefined;
        readonly existingModuleSpecifier: string;
    }
    function getExistingImportDeclarationsInfo(declarations: ReadonlyArray<AnyImportSyntax>): ExistingImportDeclarationsInfo {
        // It is possible that multiple import statements with the same specifier exist in the file.
        // e.g.
        //
        //     import * as ns from "foo";
        //     import { member1, member2 } from "foo";
        //
        //     member3/**/ <-- cusor here
        //
        // in this case we should provie 2 actions:
        //     1. change "member3" to "ns.member3"
        //     2. add "member3" to the second import statement's import list
        // and it is up to the user to decide which one fits best.
        let namespaceImportDeclaration: AnyImportSyntax | undefined;
        let namedImportDeclaration: ImportDeclaration | undefined;
        let existingModuleSpecifier: string | undefined;
        for (const declaration of declarations) {
            if (declaration.kind === SyntaxKind.ImportDeclaration) {
                const namedBindings = declaration.importClause && declaration.importClause.namedBindings;
                if (namedBindings && namedBindings.kind === SyntaxKind.NamespaceImport) {
                    // case:
                    // import * as ns from "foo"
                    namespaceImportDeclaration = declaration;
                }
                else {
                    // cases:
                    // import default from "foo"
                    // import { bar } from "foo" or combination with the first one
                    // import "foo"
                    namedImportDeclaration = declaration;
                }
                existingModuleSpecifier = declaration.moduleSpecifier.getText();
            }
            else {
                // case:
                // import foo = require("foo")
                namespaceImportDeclaration = declaration;
                const { moduleReference } = declaration;
                existingModuleSpecifier = (moduleReference.kind === SyntaxKind.ExternalModuleReference ? moduleReference.expression : moduleReference).getText();
            }
        }
        Debug.assert(existingModuleSpecifier !== undefined); // Because `declarations` should be non-empty.
        return { namespaceImportDeclaration, namedImportDeclaration, existingModuleSpecifier };
    }

    function getImportCodeActions(context: CodeFixContext): ImportCodeAction[] {
        const sourceFile = context.sourceFile;
        const allSourceFiles = context.program.getSourceFiles();
        const importFixContext = convertToImportCodeFixContext(context);

        const checker = importFixContext.checker;
        const token = importFixContext.symbolToken;
        const symbolIdActionMap = new ImportCodeActionMap();
        const currentTokenMeaning = getMeaningFromLocation(token);

        const name = importFixContext.symbolName;

        if (context.errorCode === Diagnostics._0_refers_to_a_UMD_global_but_the_current_file_is_a_module_Consider_adding_an_import_instead.code) {
            const umdSymbol = checker.getSymbolAtLocation(token);
            let symbol: ts.Symbol;
            let symbolName: string;
            if (umdSymbol.flags & ts.SymbolFlags.Alias) {
                symbol = checker.getAliasedSymbol(umdSymbol);
                symbolName = name;
            }
            else if (isJsxOpeningLikeElement(token.parent) && token.parent.tagName === token) {
                // The error wasn't for the symbolAtLocation, it was for the JSX tag itself, which needs access to e.g. `React`.
                symbol = checker.getAliasedSymbol(checker.resolveName(checker.getJsxNamespace(), token.parent.tagName, SymbolFlags.Value));
                symbolName = symbol.name;
            }
            else {
                Debug.fail("Either the symbol or the JSX namespace should be a UMD global if we got here");
            }

            importFixContext.symbolName = symbolName; //!!!!!!! TODO: fix this, make symbolName readonly
            return getCodeActionForImport(symbol, importFixContext, symbolName, ImportKind.Namespace);
        }

        const candidateModules = checker.getAmbientModules();
        for (const otherSourceFile of allSourceFiles) {
            if (otherSourceFile !== sourceFile && isExternalOrCommonJsModule(otherSourceFile)) {
                candidateModules.push(otherSourceFile.symbol);
            }
        }

        for (const moduleSymbol of candidateModules) {
            context.cancellationToken.throwIfCancellationRequested();

            // check the default export
            const defaultExport = checker.tryGetMemberInModuleExports("default", moduleSymbol);
            if (defaultExport) {
                const localSymbol = getLocalSymbolForExportDefault(defaultExport);
                if (localSymbol && localSymbol.escapedName === name && checkSymbolHasMeaning(localSymbol, currentTokenMeaning)) {
                    // check if this symbol is already used
                    const symbolId = getUniqueSymbolId(localSymbol, checker);
                    symbolIdActionMap.addActions(symbolId, getCodeActionForImport(moduleSymbol, importFixContext, name, ImportKind.Default));
                }
            }

            // "default" is a keyword and not a legal identifier for the import, so we don't expect it here
            Debug.assert(name !== "default");

            // check exports with the same name
            const exportSymbolWithIdenticalName = checker.tryGetMemberInModuleExportsAndProperties(name, moduleSymbol);
            if (exportSymbolWithIdenticalName && checkSymbolHasMeaning(exportSymbolWithIdenticalName, currentTokenMeaning)) {
                const symbolId = getUniqueSymbolId(exportSymbolWithIdenticalName, checker);
                symbolIdActionMap.addActions(symbolId, getCodeActionForImport(moduleSymbol, importFixContext, name, ImportKind.Named));
            }
        }

        return symbolIdActionMap.getAllActions();

        function checkSymbolHasMeaning(symbol: Symbol, meaning: SemanticMeaning) {
            const declarations = symbol.getDeclarations();
            return declarations ? some(symbol.declarations, decl => !!(getMeaningFromDeclaration(decl) & meaning)) : false;
        }
    }
}
