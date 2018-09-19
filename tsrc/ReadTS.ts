import * as ts from "typescript";

/** Generate documentation for all classes in a set of .ts files */
export function readTypes(
  configFilename: string,
  include: (name:string) => boolean,
): any[] {
  // Build a program using the set of root file names in fileNames
  var configJson = ts.parseConfigFileTextToJson(configFilename, ts.sys.readFile(configFilename)).config;
  var config = ts.parseJsonConfigFileContent(configJson, ts.sys, configFilename.replace(/[^/]+$/, ''), {}, configFilename);
    
  let program = ts.createProgram(config.fileNames, config.options);

  // Get the checker, we will use it to find more about classes
  let checker = program.getTypeChecker();

  let output: any[] = [];

  
  // Visit every sourceFile in the program
  for (const sourceFile of program.getSourceFiles()) {
      // Walk the tree to search for classes
      ts.forEachChild(sourceFile, visit);
  }

  return output;

  /** visit nodes finding exported classes */
  function visit(node: ts.Node) {
    // Only consider exported nodes
    if (!isNodeExported(node)) {
      return;
    }

    if (ts.isInterfaceDeclaration(node) && include(node.name.text)) {
      // This is a top level class, get its symbol
      let symbol = checker.getSymbolAtLocation(node.name);
      if (symbol) {
        let nodeType = checker.getTypeAtLocation(node);
        if (nodeType.isClassOrInterface())
        {          
          let members = convertProperties(nodeType, node);
          output.push({type: "interface", name: node.name.text, members});
        }
      }
      // No need to walk any further, class expressions/inner declarations
      // cannot be exported
    } else if (ts.isModuleDeclaration(node)) {
      // This is a namespace, visit its children
      ts.forEachChild(node, visit);
    }
  }

  function optionalMember(s: ts.Symbol, n?: ts.Node) {
    let optional = ((s.flags & ts.SymbolFlags.Optional) == ts.SymbolFlags.Optional);
    let memType = checker.getTypeOfSymbolAtLocation(s, n ? n : s.valueDeclaration);
    return {name:s.name, type: getWithAliasProps(memType), optional};
  }
  

  function convertProperties(t: ts.Type, n?: ts.Node): any {
    return t.getProperties().map( (s: ts.Symbol) => optionalMember(s, n) );
  }

  function getWithAliasProps(t: ts.Type): any {
    let alias;
    if (t.aliasSymbol)
    {
      alias = {
        "alias": {
          "typeReference": checker.getFullyQualifiedName(t.aliasSymbol),
          "typeParams": t.aliasTypeArguments ? t.aliasTypeArguments.map(getTSType):[] 
        }
      }
    } else alias = {}
    return {...alias, ...getTSType(t)}
  }

  function getTSType(memType: ts.Type): any {
    if (memType.isUnionOrIntersection()) {
      let unionTypes = memType.types.map((v: ts.Type) => getWithAliasProps(v));
      return {type: "union", types: unionTypes};
    } else if (memType.flags & (ts.TypeFlags.String
        |ts.TypeFlags.BooleanLike|ts.TypeFlags.Number
        |ts.TypeFlags.Null|ts.TypeFlags.VoidLike|ts.TypeFlags.Any))
    {
      return {type: checker.typeToString(memType)};
    }
    else if (memType.isStringLiteral())
    {
      return {type: "stringLiteral", value: memType.value}
    }
    else if (memType.isNumberLiteral())
    {
      return {type: "numberLiteral", value: memType.value}
    }
    let callSigs = memType.getCallSignatures();
    if (callSigs.length > 0)
    {
      let sig =callSigs[0];
      let params = sig.getParameters().map((p: ts.Symbol) => optionalMember(p));
      return {type:"function", params, return: getWithAliasProps(sig.getReturnType()) };
    } 
    if (memType.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive))
    {
      let objFlags = (<ts.ObjectType> memType).objectFlags;
      if (objFlags & ts.ObjectFlags.Reference)
      {
        let tr = (<ts.TypeReference> memType);
        return {
          type: "typeReference", name: checker.getFullyQualifiedName(memType.symbol), 
          typeParams: tr.typeArguments ? tr.typeArguments.map(getWithAliasProps):[], 
          flags: memType.flags, 
          objFlags
        }
      }
      else {
        if (objFlags & ts.ObjectFlags.Anonymous)
        {
          return {
            type: "object", 
            members: convertProperties( memType)
          }
        }
        if (objFlags & ts.ObjectFlags.Interface)
        {
          return {type: "interfaceReference", name: checker.getFullyQualifiedName(memType.symbol)}
        }
        return {type: "unknownObject", flags:objFlags}
      }
    }
    else if (memType.flags & ts.TypeFlags.TypeParameter)
    {
      return {type: "typeparam", name: checker.typeToString(memType)}
    }
    else {
      return {unknown: checker.typeToString(memType), flags: memType.flags}
    }
  }

  function isNodeExported(node: ts.Node): boolean {
    return (
      (ts.getCombinedNodeFlags(node) & ts.ModifierFlags.Export) !== 0 ||
      (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile)
    );
  }
}
