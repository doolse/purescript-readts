"use strict";
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
Object.defineProperty(exports, "__esModule", { value: true });
var ts = require("typescript");
function readTypes(configFilename, include) {
    var configJson = ts.parseConfigFileTextToJson(configFilename, ts.sys.readFile(configFilename)).config;
    var config = ts.parseJsonConfigFileContent(configJson, ts.sys, configFilename.replace(/[^/]+$/, ''), {}, configFilename);
    var program = ts.createProgram(config.fileNames, config.options);
    var checker = program.getTypeChecker();
    var output = [];
    for (var _i = 0, _a = program.getSourceFiles(); _i < _a.length; _i++) {
        var sourceFile = _a[_i];
        ts.forEachChild(sourceFile, visit);
    }
    return output;
    function visit(node) {
        if (!isNodeExported(node)) {
            return;
        }
        if (ts.isInterfaceDeclaration(node) && include(node.name.text)) {
            var symbol = checker.getSymbolAtLocation(node.name);
            if (symbol) {
                var nodeType = checker.getTypeAtLocation(node);
                if (nodeType.isClassOrInterface()) {
                    var members = convertProperties(nodeType, node);
                    output.push({ type: "interface", name: node.name.text, members: members });
                }
            }
        }
        else if (ts.isModuleDeclaration(node)) {
            ts.forEachChild(node, visit);
        }
    }
    function optionalMember(s, n) {
        var optional = ((s.flags & ts.SymbolFlags.Optional) == ts.SymbolFlags.Optional);
        var memType = checker.getTypeOfSymbolAtLocation(s, n ? n : s.valueDeclaration);
        return { name: s.name, type: getWithAliasProps(memType), optional: optional };
    }
    function convertProperties(t, n) {
        return t.getProperties().map(function (s) { return optionalMember(s, n); });
    }
    function getWithAliasProps(t) {
        var alias;
        if (t.aliasSymbol) {
            alias = {
                "alias": {
                    "typeReference": checker.getFullyQualifiedName(t.aliasSymbol),
                    "typeParams": t.aliasTypeArguments ? t.aliasTypeArguments.map(getTSType) : []
                }
            };
        }
        else
            alias = {};
        return __assign({}, alias, getTSType(t));
    }
    function getTSType(memType) {
        if (memType.isUnionOrIntersection()) {
            var unionTypes = memType.types.map(function (v) { return getWithAliasProps(v); });
            return { type: "union", types: unionTypes };
        }
        else if (memType.flags & (ts.TypeFlags.String
            | ts.TypeFlags.BooleanLike | ts.TypeFlags.Number
            | ts.TypeFlags.Null | ts.TypeFlags.VoidLike | ts.TypeFlags.Any)) {
            return { type: checker.typeToString(memType) };
        }
        else if (memType.isStringLiteral()) {
            return { type: "stringLiteral", value: memType.value };
        }
        else if (memType.isNumberLiteral()) {
            return { type: "numberLiteral", value: memType.value };
        }
        var callSigs = memType.getCallSignatures();
        if (callSigs.length > 0) {
            var sig = callSigs[0];
            var params = sig.getParameters().map(function (p) { return optionalMember(p); });
            return { type: "function", params: params, return: getWithAliasProps(sig.getReturnType()) };
        }
        if (memType.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)) {
            var objFlags = memType.objectFlags;
            if (objFlags & ts.ObjectFlags.Reference) {
                var tr = memType;
                return {
                    type: "typeReference", name: checker.getFullyQualifiedName(memType.symbol),
                    typeParams: tr.typeArguments ? tr.typeArguments.map(getWithAliasProps) : [],
                    flags: memType.flags,
                    objFlags: objFlags
                };
            }
            else {
                if (objFlags & ts.ObjectFlags.Anonymous) {
                    return {
                        type: "object",
                        members: convertProperties(memType)
                    };
                }
                if (objFlags & ts.ObjectFlags.Interface) {
                    return { type: "interfaceReference", name: checker.getFullyQualifiedName(memType.symbol) };
                }
                return { type: "unknownObject", flags: objFlags };
            }
        }
        else if (memType.flags & ts.TypeFlags.TypeParameter) {
            return { type: "typeparam", name: checker.typeToString(memType) };
        }
        else {
            return { unknown: checker.typeToString(memType), flags: memType.flags };
        }
    }
    function isNodeExported(node) {
        return ((ts.getCombinedNodeFlags(node) & ts.ModifierFlags.Export) !== 0 ||
            (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile));
    }
}
exports.readTypes = readTypes;
//# sourceMappingURL=ReadTS.js.map