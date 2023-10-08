namespace CSharpToSwift;

using System;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

partial class Transpiler {
    void TranspileClass(string swiftName, ClassDeclarationSyntax node, INamedTypeSymbol symbol, SemanticModel model, string indent, TextWriter w)
    {
        w.Write($"{indent}class {swiftName}");
        var head = " : ";
        if (symbol.BaseType is {} baseType && !(baseType.Name == "Object" && baseType.ContainingNamespace.Name == "System")) {
            var baseSwiftName = GetSwiftTypeName(baseType);
            w.Write($"{head}{baseSwiftName}");
            head = ", ";
        }
        foreach (var i in symbol.Interfaces) {
            var baseSwiftName = GetSwiftTypeName(i);
            w.Write($"{head}{baseSwiftName}");
            head = ", ";
        }
        w.WriteLine($" {{");
        foreach (var member in node.Members) {
            TranspileClassOrStructMember(member, swiftName, node, symbol, model, indent + "    ", w, requireMethodBody: true);
        }
        w.WriteLine($"{indent}}}");
    }

    void TranspileInterface(string swiftName, InterfaceDeclarationSyntax node, INamedTypeSymbol symbol, SemanticModel model, string indent, TextWriter w)
    {
        w.Write($"{indent}protocol {swiftName}");
        var head = " : ";
        foreach (var i in symbol.Interfaces) {
            var baseSwiftName = GetSwiftTypeName(i);
            w.Write($"{head}{baseSwiftName}");
            head = ", ";
        }
        w.WriteLine($" {{");
        foreach (var member in node.Members) {
            TranspileClassOrStructMember(member, swiftName, node, symbol, model, indent + "    ", w, requireMethodBody: false);
        }
        w.WriteLine($"{indent}}}");
    }

    void TranspileStruct(string swiftName, StructDeclarationSyntax node, INamedTypeSymbol symbol, SemanticModel model, string indent, TextWriter w)
    {
        w.Write($"{indent}struct {swiftName}");
        var head = " : ";
        foreach (var i in symbol.Interfaces) {
            var baseSwiftName = GetSwiftTypeName(i);
            w.Write($"{head}{baseSwiftName}");
            head = ", ";
        }
        w.WriteLine($" {{");
        foreach (var member in node.Members) {
            TranspileClassOrStructMember(member, swiftName, node, symbol, model, indent + "    ", w, requireMethodBody: true);
        }
        w.WriteLine($"{indent}}}");
    }

    void TranspileClassOrStructMember(MemberDeclarationSyntax member, string typeName, TypeDeclarationSyntax node, INamedTypeSymbol typeSymbol, SemanticModel model, string indent, TextWriter w, bool requireMethodBody)
    {
        switch (member.Kind ()) {
            case SyntaxKind.ClassDeclaration:
                var classDecl = (ClassDeclarationSyntax)member;
                if (model.GetDeclaredSymbol(classDecl) is INamedTypeSymbol classSymbol) {
                    var classSwiftName = GetSwiftTypeName(classSymbol);
                    TranspileClass(classSwiftName, classDecl, classSymbol, model, indent, w);
                }
                else {
                    Error($"Unable to get symbol for class: {classDecl.Identifier.Text}");
                    w.WriteLine($"{indent}/*{classDecl.ToString().Trim()}*/");
                }
                break;
            case SyntaxKind.ConstructorDeclaration:
                TranspileCtor((ConstructorDeclarationSyntax)member, typeSymbol, model, indent, w, requireMethodBody: requireMethodBody);
                break;
            case SyntaxKind.FieldDeclaration:
                TranspileField((FieldDeclarationSyntax)member, typeSymbol, model, indent, w);
                break;
            case SyntaxKind.MethodDeclaration:
                TranspileMethod((MethodDeclarationSyntax)member, typeSymbol, model, indent, w, requireMethodBody: requireMethodBody);
                break;
            case SyntaxKind.PropertyDeclaration:
                TranspileProperty((PropertyDeclarationSyntax)member, typeSymbol, model, indent, w, requireMethodBody: requireMethodBody);
                break;
            case SyntaxKind.StructDeclaration:
                var structDecl = (StructDeclarationSyntax)member;
                if (model.GetDeclaredSymbol(structDecl) is INamedTypeSymbol structSymbol) {
                    var structSwiftName = GetSwiftTypeName(structSymbol);
                    TranspileStruct(structSwiftName, structDecl, structSymbol, model, indent, w);
                }
                else {
                    Error($"Unable to get symbol for struct: {structDecl.Identifier.Text}");
                    w.WriteLine($"{indent}/*{structDecl.ToString().Trim()}*/");
                }
                break;
            default:
                Error($"Unsupported member: {member.Kind()}");
                w.WriteLine($"    /*{member.Kind()}: {member.ToString().Trim()}*/");
                break;
        }
    }

    void TranspileField(FieldDeclarationSyntax field, INamedTypeSymbol containerTypeSymbol, SemanticModel model, string indent, TextWriter w)
    {
        var docs = GetDocs(field);
        var type = model.GetSymbolInfo(field.Declaration.Type).Symbol;
        
        var ftypeName = GetSwiftTypeName(type);
        var isReadOnly = field.Modifiers.Any(x => x.IsKind(SyntaxKind.ReadOnlyKeyword));
        var isStatic = field.Modifiers.Any(x => x.IsKind(SyntaxKind.StaticKeyword));
        var decl = isReadOnly ? (isStatic ? "static let" : "let") : (isStatic ? "static var" : "var");
        
        foreach (var v in field.Declaration.Variables)
        {
            var fieldSymbol = model.GetDeclaredSymbol(v);
            var acc = GetAccessLevelModifier(fieldSymbol);
            var vn = v.Identifier.ToString();
            var initCode = v.Initializer is not null ? TranspileExpression(v.Initializer.Value, model, $"{indent}    ") : null;
            if (initCode is null && !isReadOnly)
                initCode = GetDefaultValue(type);
            var typeSuffix = "";//TODO: Enable null checking. initCode == "nil" ? "?" : "";
            if (initCode is not null)
                initCode = " = " + initCode;
            if (docs.Length > 0)
                w.WriteLine($"{indent}/// {docs}");
            w.WriteLine($"{indent}{acc}{decl} {vn}: {ftypeName}{typeSuffix}{initCode}");
        }
    }

    void TranspileCtor(ConstructorDeclarationSyntax ctor, INamedTypeSymbol containerTypeSymbol, SemanticModel model, string indent, TextWriter w, bool requireMethodBody)
    {
        var docs = GetDocs(ctor);
        if (docs.Length > 0)
            w.WriteLine($"{indent}/// {docs}");
        var methodSymbol = model.GetDeclaredSymbol(ctor);
        var acc = GetAccessLevelModifier(methodSymbol);
        var isStatic = ctor.Modifiers.Any(x => x.IsKind(SyntaxKind.StaticKeyword));
        var slotType = isStatic ? "static " : "";
        w.Write($"{indent}{acc}{slotType}init(");
        TranspileParams(ctor.ParameterList, model, w);
        if (ctor.Body is null && !requireMethodBody) {
            w.WriteLine($")");
        }
        else {
            w.WriteLine($") {{");
            if (ctor.Body is {} block) {
                TranspileBlock(block, model, $"{indent}    ", w);
            }
            w.WriteLine($"{indent}}}");
        }
    }

    void TranspileMethod(MethodDeclarationSyntax method, INamedTypeSymbol containerTypeSymbol, SemanticModel model, string indent, TextWriter w, bool requireMethodBody)
    {
        var docs = GetDocs(method);
        if (docs.Length > 0)
            w.WriteLine($"{indent}/// {docs}");
        var returnType = model.GetSymbolInfo(method.ReturnType).Symbol;
        var isVoid = IsTypeVoid(returnType);
        var returnTypeCode = isVoid ? "" : $" -> {GetSwiftTypeName(returnType)}";
        var methodSymbol = model.GetDeclaredSymbol(method);
        var acc = GetAccessLevelModifier(methodSymbol);
        var isStatic = method.Modifiers.Any(x => x.IsKind(SyntaxKind.StaticKeyword));
        var isOverride = method.Modifiers.Any(x => x.IsKind(SyntaxKind.OverrideKeyword));
        var isSealed = method.Modifiers.Any(x => x.IsKind(SyntaxKind.SealedKeyword));
        var isAbstract = method.Modifiers.Any(x => x.IsKind(SyntaxKind.AbstractKeyword));
        var isVirtual = method.Modifiers.Any(x => x.IsKind(SyntaxKind.VirtualKeyword));
        if (isAbstract)
        {
            // Warning("Abstract methods are not supported");
        }
        // acc = acc + $"/*{method.Modifiers}*/";
        var slotType = isStatic ? "static " : (isOverride ? "override " : (isAbstract ? "/*abstract*/ " : (isVirtual ? "" : (method.Body is not null ? "final " : ""))));
        w.Write($"{indent}{acc}{slotType}func {method.Identifier.ToString()}(");
        TranspileParams(method.ParameterList, model, w);
        w.Write($"){returnTypeCode}");
        if (method.Body is null && !requireMethodBody) {
            w.WriteLine();
        }
        else {
            w.WriteLine($" {{");
            if (method.Body is {} block) {
                TranspileBlock(block, model, $"{indent}    ", w);
            }
            w.WriteLine($"{indent}}}");
        }
    }

    private void TranspileParams(ParameterListSyntax parameterList, SemanticModel model, TextWriter w)
    {
        var head = "";
        foreach (var p in parameterList.Parameters)
        {
            var pname = p.Identifier.ToString();
            if (p.Type is null) {
                Error($"Parameter has no type: {pname}");
                w.Write($"{head}{pname}: Int/*Error: not type*/");
            }
            else {
                var ptypeSymbol = model.GetSymbolInfo(p.Type).Symbol;
                var isArray = IsTypeArray(ptypeSymbol);
                var ptypeName = GetSwiftTypeName(ptypeSymbol);
                var refMod = isArray ? "inout " : "";
                w.Write($"{head}{pname}: {refMod}{ptypeName}");
            }
            head = ", ";
        }
    }

    void TranspileProperty(PropertyDeclarationSyntax prop, INamedTypeSymbol containerTypeSymbol, SemanticModel model, string indent, TextWriter w, bool requireMethodBody)
    {
        var docs = GetDocs(prop);
        if (docs.Length > 0)
            w.WriteLine($"{indent}/// {docs}");
        var returnType = model.GetSymbolInfo(prop.Type).Symbol;
        string slotType = GetSlotTypeModifier(prop);
        var vn = prop.Identifier.ToString();
        var initCode = prop.Initializer is not null ? TranspileExpression(prop.Initializer.Value, model) : null;
        if (initCode is not null)
            initCode = " = " + initCode;
        w.WriteLine($"{indent}{slotType}var {vn}: {GetSwiftTypeName(returnType)}{initCode} {{");
        if (prop.AccessorList is { } alist)
        {
            foreach (var accessor in alist.Accessors)
            {
                var accLevel = GetAccessLevelModifier(accessor, model);
                w.Write($"{indent}    {accessor.Keyword}");
                if (accessor.Body is null && accessor.ExpressionBody is null && !requireMethodBody) {
                    w.WriteLine();
                }
                else {
                    w.WriteLine($" {{");
                    if (accessor.Body is {} block) {
                        TranspileBlock(block, model, $"{indent}        ", w);
                    }
                    else if (accessor.ExpressionBody is {} ebody) {
                        var eCode = TranspileExpression(ebody.Expression, model, $"{indent}        ");
                        w.WriteLine($"{indent}        {eCode}");
                    }
                    w.WriteLine($"{indent}    }}");
                }
            }
        }
        else if (prop.ExpressionBody is {} ebody) {
            var eCode = TranspileExpression(ebody.Expression, model, $"{indent}    ");
            w.WriteLine($"{indent}    get {{ {eCode} }}");
        }
        else {
            Error($"Property has no accessors: {vn}");
            w.WriteLine($"{indent}    // No accessors");
        }
        w.WriteLine($"{indent}}}");
    }

        string GetSwiftTypeName(CSharpSyntaxNode type, SemanticModel model)
    {
        var typeSymbol = model.GetSymbolInfo(type).Symbol;
        return GetSwiftTypeName(typeSymbol);
    }

    string GetSwiftTypeName(ISymbol? s)
    {
        if (s == null) {
            Error($"No symbol type provided");
            return "AnyObject/*no symbol*/";
        }
        else if (s is IArrayTypeSymbol ats) {
            return $"[{GetSwiftTypeName(ats.ElementType)}]";
        }
        else if (s is INamedTypeSymbol nts) {
            var name = nts.Name;
            switch (name) {
                case nameof(System.Boolean):
                    return "Bool";
                case nameof(System.Byte):
                    return "UInt8";
                case nameof(System.Char):
                    return "Character";
                case nameof(System.Int32):
                    return "Int";
                case nameof(System.IntPtr):
                    return "Int";
                case nameof(System.Object):
                    return "AnyObject";
                case nameof(System.Single):
                    return "Float";
                default:
                    if (string.IsNullOrEmpty(name)) {
                        Error($"No name for symbol: {s.GetType()}");
                        return "AnyObject/*no name*/";
                    }
                    else if (name == "Nullable" && s.ContainingNamespace.Name == "System") {
                        return GetSwiftTypeName(nts.TypeArguments.First()) + "?";
                    }
                    return name;
            }
        }
        else if (s is ITypeParameterSymbol tps) {
            return tps.Name;
        }
        else {
            Error($"Unsupported type symbol: {s.GetType()}");
            return $"AnyObject/*{s}*/";
        }
    }

    bool IsTypeVoid(ISymbol? typeSymbol)
    {
        return typeSymbol is null || (typeSymbol.Name == "Void" && typeSymbol.ContainingNamespace.Name == "System");
    }

    static bool IsTypeArray(ISymbol? typeSymbol)
    {
        return typeSymbol is IArrayTypeSymbol;
    }
}
