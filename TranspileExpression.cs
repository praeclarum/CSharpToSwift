namespace CSharpToSwift;

using System;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

partial class Transpiler {
    string TranspileExpression(ExpressionSyntax value, SemanticModel model, string indent = "")
    {
        switch (value.Kind ()) {
            case SyntaxKind.AddExpression:
                var add = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(add.Left, model)} + {TranspileExpression(add.Right, model)}";
            case SyntaxKind.AddAssignmentExpression:
                var addAssign = (AssignmentExpressionSyntax)value;
                return $"{TranspileExpression(addAssign.Left, model)} += {TranspileExpression(addAssign.Right, model)}";
            case SyntaxKind.AndAssignmentExpression:
                var andAssign = (AssignmentExpressionSyntax)value;
                return $"{TranspileExpression(andAssign.Left, model)} &= {TranspileExpression(andAssign.Right, model)}";
            case SyntaxKind.ArrayCreationExpression:
                return TranspileArrayCreation((ArrayCreationExpressionSyntax)value, model, indent);
            case SyntaxKind.ArrayInitializerExpression:
                var aiElements = string.Join(", ", ((InitializerExpressionSyntax)value).Expressions.Select(x => TranspileExpression(x, model, indent)));
                return $"[{aiElements}]";
            case SyntaxKind.AsExpression:
                var ase = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(ase.Left, model)} as? {TranspileExpression(ase.Right, model)}";
            case SyntaxKind.BaseExpression:
                return "super";
            case SyntaxKind.BitwiseAndExpression:
                var bitAnd = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(bitAnd.Left, model)} & {TranspileExpression(bitAnd.Right, model)}";
            case SyntaxKind.BitwiseOrExpression:
                var bitOr = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(bitOr.Left, model)} | {TranspileExpression(bitOr.Right, model)}";
            case SyntaxKind.BitwiseNotExpression:
                var bitNot = (PrefixUnaryExpressionSyntax)value;
                return $"~{TranspileExpression(bitNot.Operand, model)}";
            case SyntaxKind.CastExpression:
                var cast = (CastExpressionSyntax)value;
                return $"{TranspileExpression(cast.Expression, model)} as {GetSwiftTypeName (cast.Type, model)}";
            case SyntaxKind.CharacterLiteralExpression:
                var charLit = (LiteralExpressionSyntax)value;
                return charLit.Token.Text.Replace('\'', '\"');
            case SyntaxKind.CoalesceExpression:
                var coalesce = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(coalesce.Left, model)} ?? {TranspileExpression(coalesce.Right, model)}";
            case SyntaxKind.ConditionalAccessExpression:
                var ca = (ConditionalAccessExpressionSyntax)value;
                return $"{TranspileExpression(ca.Expression, model)}?{TranspileExpression(ca.WhenNotNull, model)}";
            case SyntaxKind.ConditionalExpression:
                var cond = (ConditionalExpressionSyntax)value;
                return $"{TranspileExpression(cond.Condition, model)} ? {TranspileExpression(cond.WhenTrue, model)} : {TranspileExpression(cond.WhenFalse, model)}";
            case SyntaxKind.DivideExpression:
                var div = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(div.Left, model)} / {TranspileExpression(div.Right, model)}";
            case SyntaxKind.DefaultExpression:
                var def = (DefaultExpressionSyntax)value;
                return GetDefaultValue(model.GetTypeInfo(def.Type).Type);
            case SyntaxKind.DivideAssignmentExpression:
                var divAssign = (AssignmentExpressionSyntax)value;
                return $"{TranspileExpression(divAssign.Left, model)} /= {TranspileExpression(divAssign.Right, model)}";
            case SyntaxKind.ElementAccessExpression:
                var ea = (ElementAccessExpressionSyntax)value;
                var eaArgs = TranspileElementArguments(ea.ArgumentList, model, indent);
                return $"{TranspileExpression(ea.Expression, model)}[{eaArgs}]";
            case SyntaxKind.ElementBindingExpression:
                var eb = (ElementBindingExpressionSyntax)value;
                var ebArgs = TranspileElementArguments(eb.ArgumentList, model, indent);
                return $"[{ebArgs}]";
            case SyntaxKind.EqualsExpression:
                var eq = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(eq.Left, model)} == {TranspileExpression(eq.Right, model)}";
            case SyntaxKind.ExclusiveOrExpression:
                var xor = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(xor.Left, model)} ^ {TranspileExpression(xor.Right, model)}";
            case SyntaxKind.FalseLiteralExpression:
                return "false";
            case SyntaxKind.GreaterThanExpression:
                var gt = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(gt.Left, model)} > {TranspileExpression(gt.Right, model)}";
            case SyntaxKind.GreaterThanOrEqualExpression:
                var gte = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(gte.Left, model)} >= {TranspileExpression(gte.Right, model)}";
            case SyntaxKind.IdentifierName:
                var id = (IdentifierNameSyntax)value;
                return id.Identifier.ToString();
            case SyntaxKind.ImplicitArrayCreationExpression:
                var iac = (ImplicitArrayCreationExpressionSyntax)value;
                var iacArgs = string.Join(", ", iac.Initializer.Expressions.Select(e => TranspileExpression(e, model, indent)));
                return $"[{iacArgs}]";
            case SyntaxKind.InvocationExpression:
                var inv = (InvocationExpressionSyntax)value;
                return TranspileInvocation(TranspileExpression(inv.Expression, model), inv, inv.ArgumentList, model, indent);
            case SyntaxKind.IsExpression:
                var ise = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(ise.Left, model)} is {TranspileExpression(ise.Right, model)}";
            case SyntaxKind.IsPatternExpression:
                return TranspileIsPattern((IsPatternExpressionSyntax)value, model, indent);
            case SyntaxKind.LeftShiftExpression:
                var lshift = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(lshift.Left, model)} << {TranspileExpression(lshift.Right, model)}";
            case SyntaxKind.LeftShiftAssignmentExpression:
                var lshiftAssign = (AssignmentExpressionSyntax)value;
                return $"{TranspileExpression(lshiftAssign.Left, model)} <<= {TranspileExpression(lshiftAssign.Right, model)}";
            case SyntaxKind.LessThanExpression:
                var lt = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(lt.Left, model)} < {TranspileExpression(lt.Right, model)}";
            case SyntaxKind.LessThanOrEqualExpression:
                var lte = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(lte.Left, model)} <= {TranspileExpression(lte.Right, model)}";
            case SyntaxKind.LogicalAndExpression:
                var and = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(and.Left, model)} && {TranspileExpression(and.Right, model)}";
            case SyntaxKind.LogicalNotExpression:
                var not = (PrefixUnaryExpressionSyntax)value;
                return $"!{TranspileExpression(not.Operand, model)}";
            case SyntaxKind.LogicalOrExpression:
                var or = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(or.Left, model)} || {TranspileExpression(or.Right, model)}";
            case SyntaxKind.MemberBindingExpression:
                var mbe = (MemberBindingExpressionSyntax)value;
                return $".{TranspileExpression(mbe.Name, model)}";
            case SyntaxKind.ModuloExpression:
                var mod = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(mod.Left, model)} % {TranspileExpression(mod.Right, model)}";
            case SyntaxKind.MultiplyExpression:
                var mul = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(mul.Left, model)} * {TranspileExpression(mul.Right, model)}";
            case SyntaxKind.MultiplyAssignmentExpression:
                var mulAssign = (AssignmentExpressionSyntax)value;
                return $"{TranspileExpression(mulAssign.Left, model)} *= {TranspileExpression(mulAssign.Right, model)}";
            case SyntaxKind.NotEqualsExpression:
                var neq = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(neq.Left, model)} != {TranspileExpression(neq.Right, model)}";
            case SyntaxKind.NullLiteralExpression:
                return "nil";
            case SyntaxKind.NumericLiteralExpression:
                var nlit = (LiteralExpressionSyntax)value;
                {
                    var ntext = nlit.Token.Text;
                    if (ntext[0] == '.')
                        ntext = "0" + ntext;
                    if (ntext[^1] == 'f')
                        ntext = ntext.Substring(0, ntext.Length - 1);
                    if (ntext.EndsWith("ul", StringComparison.InvariantCultureIgnoreCase))
                        ntext = $"UInt64({ntext.Substring(0, ntext.Length - 2)})";
                    return ntext;
                }
            case SyntaxKind.ObjectCreationExpression:
                return TranspileObjectCreationExpression((ObjectCreationExpressionSyntax)value, model, indent);
            case SyntaxKind.OrAssignmentExpression:
                var orAssign = (AssignmentExpressionSyntax)value;
                return $"{TranspileExpression(orAssign.Left, model)} |= {TranspileExpression(orAssign.Right, model)}";
            case SyntaxKind.ParenthesizedExpression:
                var paren = (ParenthesizedExpressionSyntax)value;
                return $"({TranspileExpression(paren.Expression, model)})";
            case SyntaxKind.ParenthesizedLambdaExpression:
                var parenLambda = (ParenthesizedLambdaExpressionSyntax)value;
                return TranspileLambdaExpression(parenLambda.ParameterList.Parameters, parenLambda, model, indent);
            case SyntaxKind.PostDecrementExpression:
                var postDec = (PostfixUnaryExpressionSyntax)value;
                return $"{TranspileExpression(postDec.Operand, model)} -= 1";
            case SyntaxKind.PostIncrementExpression:
                var postInc = (PostfixUnaryExpressionSyntax)value;
                return $"{TranspileExpression(postInc.Operand, model)} += 1";
            case SyntaxKind.PredefinedType:
                return GetSwiftTypeName(value, model);
            case SyntaxKind.PreDecrementExpression:
                var preDec = (PrefixUnaryExpressionSyntax)value;
                return $"{TranspileExpression(preDec.Operand, model)} -= 1";
            case SyntaxKind.PreIncrementExpression:
                var preInc = (PrefixUnaryExpressionSyntax)value;
                return $"{TranspileExpression(preInc.Operand, model)} += 1";
            case SyntaxKind.RangeExpression:
                var range = (RangeExpressionSyntax)value;
                var rangeStart = range.LeftOperand is not null ? TranspileExpression(range.LeftOperand, model) : null;
                var rangeEnd = range.RightOperand is not null ? TranspileExpression(range.RightOperand, model) : null;
                if (rangeEnd is not null)
                    return $"{rangeStart}..<{rangeEnd}";
                else
                    return $"{rangeStart}...";
            case SyntaxKind.RightShiftExpression:
                var rshift = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(rshift.Left, model)} >> {TranspileExpression(rshift.Right, model)}";
            case SyntaxKind.RightShiftAssignmentExpression:
                var rshiftAssign = (AssignmentExpressionSyntax)value;
                return $"{TranspileExpression(rshiftAssign.Left, model)} >>= {TranspileExpression(rshiftAssign.Right, model)}";
            case SyntaxKind.SimpleAssignmentExpression:
                var sae = (AssignmentExpressionSyntax)value;
                return $"{TranspileExpression(sae.Left, model)} = {TranspileExpression(sae.Right, model)}";
            case SyntaxKind.SimpleLambdaExpression:
                var simpleLambda = (SimpleLambdaExpressionSyntax)value;
                return TranspileLambdaExpression(new[]{simpleLambda.Parameter}, simpleLambda, model, indent);
            case SyntaxKind.SimpleMemberAccessExpression:
                var sma = (MemberAccessExpressionSyntax)value;
                return $"{TranspileExpression(sma.Expression, model)}.{sma.Name.ToString()}";
            case SyntaxKind.StringLiteralExpression:
                var slit = (LiteralExpressionSyntax)value;
                {
                    var stext = slit.Token.Text;
                    if (stext.Length > 0 && stext[0] == '@') {
                        stext = "\"\"\"\n" + slit.Token.ValueText + "\n\"\"\"";
                    }
                    return stext;
                }
            case SyntaxKind.SubtractAssignmentExpression:
                var subAssign = (AssignmentExpressionSyntax)value;
                return $"{TranspileExpression(subAssign.Left, model)} -= {TranspileExpression(subAssign.Right, model)}";
            case SyntaxKind.SubtractExpression:
                var sub = (BinaryExpressionSyntax)value;
                return $"{TranspileExpression(sub.Left, model)} - {TranspileExpression(sub.Right, model)}";
            case SyntaxKind.ThisExpression:
                return "self";
            case SyntaxKind.TrueLiteralExpression:
                return "true";
            case SyntaxKind.UnaryMinusExpression:
                var ume = (PrefixUnaryExpressionSyntax)value;
                return $"-{TranspileExpression(ume.Operand, model)}";
            default:
                Error($"Unsupported expression: {value.Kind()}");
                return $"nil/*{value.Kind()}: {value.ToString().Trim()}*/";
        }
    }

    string TranspileIsPattern(IsPatternExpressionSyntax value, SemanticModel model, string indent)
    {
        var exprCode = TranspileExpression(value.Expression, model, indent);
        switch (value.Pattern.Kind ()) {
            case SyntaxKind.DeclarationPattern:
                var dp = (DeclarationPatternSyntax)value.Pattern;
                var dpTypeName = GetSwiftTypeName(dp.Type, model);
                if (dp.Designation is SingleVariableDesignationSyntax svds) {
                    return $"let {svds.Identifier} = {exprCode} as? {dpTypeName}";
                }
                else {
                    Error($"Unsupported declaration pattern: {dp.Designation.ToString()}");
                    return $"{exprCode}/* is {dp.Designation.ToString()}*/";
                }
            case SyntaxKind.ConstantPattern:
                var cp = (ConstantPatternSyntax)value.Pattern;
                switch (cp.Expression.Kind()) {
                    case SyntaxKind.TrueLiteralExpression:
                        return $"{exprCode}";
                    case SyntaxKind.FalseLiteralExpression:
                        return $"!{exprCode}";
                    case SyntaxKind.NullLiteralExpression:
                        return $"{exprCode} == nil";
                    default:
                        return $"{exprCode} == {TranspileExpression(cp.Expression, model)}";
                }
            default:
                Error($"Unsupported pattern: {value.Pattern.Kind()}");
                return $"{exprCode}/*{value.Pattern.Kind()}: {value.Pattern.ToString().Trim()}*/";
        }
    }

    private string TranspileArrayCreation(ArrayCreationExpressionSyntax array, SemanticModel model, string indent)
    {
        var etypeSymbol = model.GetTypeInfo(array.Type.ElementType).Type;
        if (array.Type.RankSpecifiers.Count == 1 && array.Type.RankSpecifiers[0].Sizes is {Count:1} sizes) {
            var lengthExpr = sizes[0];
            var length = TryEvaluateConstantIntExpression(lengthExpr, model, 0);
            if (array.Initializer is {} init) {
                var sb = new System.Text.StringBuilder();
                sb.Append("[");
                var head = "";
                var num = 0;
                foreach (var e in init.Expressions) {
                    sb.Append(head);
                    sb.Append(TranspileExpression(e, model, indent));
                    head = ", ";
                    num++;
                }
                if (num < length) {
                    var valueCode = GetDefaultValue(etypeSymbol);
                    for (var i = num; i < length; i++) {
                        sb.Append(head);
                        sb.Append(valueCode);
                        head = ", ";
                    }
                }
                sb.Append("]");
                return sb.ToString();
            }
            else  {
                var valueCode = GetDefaultValue(etypeSymbol);
                var lengthCode = TranspileExpression(lengthExpr, model);
                return $"Array(repeating: {valueCode}, count: {lengthCode})";
            }
        }
        else {
            Error($"Unsupported array creation: {array.ToString().Trim()}");
            return $"[]/*{array.ToString().Trim()}*/";
        }
    }

    string TranspileElementArguments(BracketedArgumentListSyntax argList, SemanticModel model, string indent)
    {
        return string.Join(", ", argList.Arguments.Select(x => TranspileExpression(x.Expression, model, indent)));
    }

    string TranspileLambdaExpression(IEnumerable<ParameterSyntax> parameters, LambdaExpressionSyntax lambda, SemanticModel model, string indent)
    {
        var parametersCode = string.Join(", ", parameters.Select(x => x.Identifier.ToString()));
        var sb = new System.Text.StringBuilder();
        sb.Append($"{{ {parametersCode} in");
        if (lambda.ExpressionBody is {} expr) {
            var exprCode = TranspileExpression(expr, model);
            sb.Append($" {exprCode} }}");
        }
        else if (lambda.Block is {} block) {
            using var sw = new StringWriter();
            TranspileBlock(block, model, indent, sw);
            var blockCode = sw.ToString();
            sb.Append($"\n{blockCode}{indent}}}");
        }
        else {
            Error($"Unsupported lambda expression: {lambda.ToString().Trim()}");
            sb.Append($" nil/*{lambda.ToString().Trim()}*/ }}");
        }
        return sb.ToString();
    }

    string TranspileObjectCreationExpression(ObjectCreationExpressionSyntax oc, SemanticModel model, string indent)
    {
        if (model.GetSymbolInfo(oc.Type).Symbol is ITypeSymbol ocTypeSymbol)
        {
            var ocInit = oc.Initializer;
            if (ocTypeSymbol.Name == "Dictionary" && ocTypeSymbol.ContainingNamespace.ToString() == "System.Collections.Generic") {
                var sb = new System.Text.StringBuilder();
                sb.Append("[");
                if (ocInit is not null && ocInit.Expressions.Count > 0)
                {
                    var head = "";
                    foreach (var kv in ocInit.Expressions)
                    {
                        sb.Append(head);
                        if (kv is InitializerExpressionSyntax ie && ie.Expressions.Count == 2) {
                            sb.Append($"{TranspileExpression(ie.Expressions[0], model)}: {TranspileExpression(ie.Expressions[1], model)}");
                        }
                        else {
                            Error($"Unsupported dictionary initializer: {kv.Kind()}");
                            sb.Append($"nil/*{kv.ToString().Trim()}*/");
                        }
                        head = ", ";
                    }
                }
                sb.Append("]");
                return sb.ToString();
            }
            else {
                var ocName = GetSwiftTypeName(ocTypeSymbol);
                var ocCode = oc.ArgumentList != null ? TranspileInvocation(ocName, oc, oc.ArgumentList, model, indent) : $"{ocName}()";
                if (ocInit is not null && ocInit.Expressions.Count > 0)
                {
                    var kw = ocTypeSymbol.IsReferenceType ? "let" : "var";
                    var inits = ocInit.Expressions.Select(x => $"x.{TranspileExpression(x, model)}");
                    var initsCode = string.Join("; ", inits);
                    ocCode = $"{{ {kw} x = {ocCode}; {initsCode} }}()";
                }
                return ocCode;
            }
        }
        else
        {
            Error($"Unable to determine type of object creation expression: {oc.Type.ToString()}");
            return $"nil/*no type symbol: {oc.Type.ToString().Trim()}*/";
        }
    }

    string TranspileInvocation(string exprCode, SyntaxNode invokeNode, ArgumentListSyntax argList, SemanticModel model, string indent)
    {
        if (exprCode == "nameof" && argList.Arguments.Count == 1)
            return NameOf(argList.Arguments[0].Expression);
        var method = model.GetSymbolInfo(invokeNode).Symbol as IMethodSymbol;
        if (method == null) {
            Error($"Method resolution failed: {invokeNode}");
            var fargs = argList.Arguments.Select(a => TranspileExpression(a.Expression, model)).ToArray();
            return $"{exprCode}({string.Join(", ", fargs)})";
        }
        var parameters = method.Parameters;
        if (parameters.Length == 0) {
            return $"{exprCode}()";
        }
        var args = argList.Arguments;
        var nparams = parameters.Length;
        var nargs = args.Count;
        var sb = new System.Text.StringBuilder();
        var aindent = $"{indent}    ";
        sb.Append(exprCode);
        sb.Append("(");
        for (var i = 0; i < nparams; i++) {
            var paramName = parameters[i].Name;
            if (i > 0)
                sb.Append(", ");
            sb.Append(paramName);
            sb.Append(": ");
            if (i < nargs) {
                var arg = args[i];
                sb.Append(TranspileExpression(arg.Expression, model, aindent));
            } else {
                Error("Missing argument value");
                sb.Append("nil/*missing*/");
            }
        }
        if (nargs > nparams) {
            Error("Too many arguments");
            for (var i = nparams; i < nargs; i++) {
                var arg = args[i];
                sb.Append(", ");
                sb.Append(TranspileExpression(arg.Expression, model, aindent));
                sb.Append("/*extra*/");
            }
        }
        sb.Append(")");
        return sb.ToString();
    }

    string NameOf(ExpressionSyntax value)
    {
        var name = value switch {
            IdentifierNameSyntax id => id.Identifier.ToString(),
            MemberAccessExpressionSyntax mae => mae.Name.ToString(),
            _ => value.ToString()
        };
        return $"\"{name}\"";
    }

        int TryEvaluateConstantIntExpression(ExpressionSyntax expr, SemanticModel model, int defaultInt)
    {
        if (expr is LiteralExpressionSyntax lit && lit.IsKind(SyntaxKind.NumericLiteralExpression)) {
            return int.Parse(lit.Token.ValueText);
        }
        return defaultInt;
    }

    string GetDefaultValue(ISymbol? type)
    {
        if (type == null) {
            return "nil";
        }
        switch (type.Kind) {
            case SymbolKind.ArrayType:
                return "[]";
            case SymbolKind.PointerType:
                return "nil";
            case SymbolKind.DynamicType:
                return "nil";
            case SymbolKind.TypeParameter:
                return "nil";
            case SymbolKind.ErrorType:
                return "nil";
            case SymbolKind.NamedType:
                var ntype = (INamedTypeSymbol)type;
                switch (type.Name) {
                    case nameof(System.Boolean):
                        return "false";
                    case nameof(System.Byte):
                        return "0";
                    case nameof(System.Char):
                        return "\"\\0\"";
                    case nameof(System.Double):
                        return "0.0";
                    case nameof(System.Single):
                        return "0.0";
                    case nameof(System.Int16):
                        return "0";
                    case nameof(System.Int32):
                        return "0";
                    case nameof(System.Int64):
                        return "0";
                    case nameof(System.IntPtr):
                        return "0";
                    case nameof(System.UInt16):
                        return "0";
                    case nameof(System.UInt32):
                        return "0";
                    case nameof(System.UInt64):
                        return "0";
                    case nameof(System.UIntPtr):
                        return "0";
                    default:
                        if (ntype.IsReferenceType) {
                            return "nil";
                        }
                        else if (ntype.Name == "Nullable" && ntype.ContainingNamespace.Name == "System") {
                            return "nil";
                        }
                        else if (ntype.Name == "Enum" && ntype.ContainingNamespace.Name == "System") {
                            return "0";
                        }
                        else if (ntype.InstanceConstructors.FirstOrDefault(x => x.Parameters.Length == 0) is IMethodSymbol defaultCtor) {
                            return $"{GetSwiftTypeName(type)}()";
                        }
                        else {
                            Error($"Unsupported default value for named type: {type.Name}");
                            return $"0/*NT:{type.Name}*/";
                        }
                }
            default:
                Error($"Unsupported default value for type {type.Kind}");
                return $"nil/*T:{type.Kind}*/";
        }
    }
}
