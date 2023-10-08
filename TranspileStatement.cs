namespace CSharpToSwift;

using System;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

partial class Transpiler {
    void TranspileBlock(BlockSyntax block, SemanticModel model, string indent, TextWriter w)
    {
        foreach (var stmt in block.Statements)
        {
            TranspileStatement(stmt, model, indent, w);
        }
    }

    void TranspileStatement(StatementSyntax stmt, SemanticModel model, string indent, TextWriter w)
    {
        switch (stmt.Kind()) {
            case SyntaxKind.Block:
                TranspileBlock((BlockSyntax)stmt, model, indent, w);
                break;
            case SyntaxKind.BreakStatement:
                w.WriteLine($"{indent}break");
                break;
            case SyntaxKind.ContinueStatement:
                w.WriteLine($"{indent}continue");
                break;
            case SyntaxKind.ExpressionStatement:
                TranspileExpressionStatement((ExpressionStatementSyntax)stmt, model, indent, w);
                break;
            case SyntaxKind.ForStatement:
                TranspileForStatement((ForStatementSyntax)stmt, model, indent, w);
                break;
            case SyntaxKind.ForEachStatement:
                TranspileForEachStatement((ForEachStatementSyntax)stmt, model, indent, w);
                break;
            case SyntaxKind.IfStatement:
                TranspileIfStatement((IfStatementSyntax)stmt, model, indent, w);
                break;
            case SyntaxKind.LocalDeclarationStatement:
                var ld = (LocalDeclarationStatementSyntax)stmt;
                TranspileVariableDeclaration(ld.Declaration, model, indent, w);
                break;
            case SyntaxKind.ReturnStatement:
                TranspileReturnStatement((ReturnStatementSyntax)stmt, model, indent, w);
                break;
            case SyntaxKind.SwitchStatement:
                TranspileSwitchStatement((SwitchStatementSyntax)stmt, model, indent, w);
                break;
            case SyntaxKind.ThrowStatement:
                var thrw = (ThrowStatementSyntax)stmt;
                if (thrw.Expression is {} expr) {
                    w.WriteLine($"{indent}throw {TranspileExpression(thrw.Expression, model)}");
                }
                else {
                    w.WriteLine($"{indent}throw");
                }
                break;
            case SyntaxKind.TryStatement:
                TranspileTryStatement((TryStatementSyntax)stmt, model, indent, w);
                break;
            case SyntaxKind.UsingStatement:
                TranspileUsingStatement((UsingStatementSyntax)stmt, model, indent, w);
                break;
            case SyntaxKind.WhileStatement:
                TranspileWhileStatement((WhileStatementSyntax)stmt, model, indent, w);
                break;
            default:
                Error($"Unsupported statement: {stmt.Kind()}");
                w.WriteLine($"{indent}/*{stmt.Kind()}: {stmt.ToString().Trim()}*/");
                break;
        }
    }

    void TranspileExpressionStatement(ExpressionStatementSyntax stmt, SemanticModel model, string indent, TextWriter w)
    {
        var expr = TranspileExpression(stmt.Expression, model);
        w.WriteLine($"{indent}{expr}");
    }

    void TranspileForStatement(ForStatementSyntax stmt, SemanticModel model, string indent, TextWriter w)
    {
        if (stmt.Declaration is {} decl) {
            TranspileVariableDeclaration(decl, model, indent, w);
        }
        var cond = stmt.Condition is not null ? TranspileExpression(stmt.Condition, model) : "true";
        w.WriteLine($"{indent}while {cond} {{");
        TranspileStatement(stmt.Statement, model, indent + "    ", w);
        foreach (var incr in stmt.Incrementors) {
            var expr = TranspileExpression(incr, model);
            w.WriteLine($"{indent}    {expr}");
        }
        w.WriteLine($"{indent}}}");
    }

    void TranspileForEachStatement(ForEachStatementSyntax stmt, SemanticModel model, string indent, TextWriter w)
    {
        var expr = stmt.Expression;
        var ident = stmt.Identifier;
        var exprCode = TranspileExpression(expr, model);
        w.WriteLine($"{indent}for {ident.ValueText} in {exprCode} {{");
        TranspileStatement(stmt.Statement, model, indent + "    ", w);
        w.WriteLine($"{indent}}}");
    }

    void TranspileIfStatement(IfStatementSyntax stmt, SemanticModel model, string indent, TextWriter w)
    {
        var cond = TranspileExpression(stmt.Condition, model);
        w.WriteLine($"{indent}if {cond} {{");
        TranspileStatement(stmt.Statement, model, indent + "    ", w);
        if (stmt.Else is { } elseClause)
        {
            w.WriteLine($"{indent}}} else {{");
            TranspileStatement(elseClause.Statement, model, indent + "    ", w);
        }
        w.WriteLine($"{indent}}}");
    }

    void TranspileReturnStatement(ReturnStatementSyntax stmt, SemanticModel model, string indent, TextWriter w)
    {
        var expr = stmt.Expression;
        if (expr is null)
            w.WriteLine($"{indent}return");
        else
            w.WriteLine($"{indent}return {TranspileExpression(expr, model)}");
    }

    void TranspileSwitchStatement(SwitchStatementSyntax stmt, SemanticModel model, string indent, TextWriter w)
    {
        var expr = stmt.Expression;
        w.WriteLine($"{indent}switch {TranspileExpression(expr, model)} {{");
        foreach (var s in stmt.Sections)
        {
            var caseLabels = s.Labels.Where(x => !x.IsKind(SyntaxKind.DefaultSwitchLabel)).ToArray();
            var defLabel = s.Labels.FirstOrDefault(x => x.IsKind(SyntaxKind.DefaultSwitchLabel));
            if (caseLabels.Length > 0) {
                var labels = string.Join(", ", s.Labels.Select(x => {
                    if (x.IsKind(SyntaxKind.DefaultSwitchLabel)) {
                        return "default";
                    } else if (x.IsKind(SyntaxKind.CaseSwitchLabel)) {
                        return TranspileExpression(((CaseSwitchLabelSyntax)x).Value, model, indent);
                    } else {
                        Error($"Unsupported switch label: {x}");
                        return x.ToString();
                    }
                }));
                w.WriteLine($"{indent}case {labels}:");
                foreach (var cstmt in s.Statements) {
                    TranspileStatement(cstmt, model, indent + "    ", w);
                }
            }
            if (defLabel is not null) {
                w.WriteLine($"{indent}default:");
                foreach (var cstmt in s.Statements) {
                    TranspileStatement(cstmt, model, indent + "    ", w);
                }
            }
        }
        w.WriteLine($"{indent}}}");
    }

    void TranspileTryStatement(TryStatementSyntax stmt, SemanticModel model, string indent, TextWriter w)
    {
        var block = stmt.Block;
        var catches = stmt.Catches;
        if (catches.Count > 0) {
            w.WriteLine($"{indent}do {{");
            if (stmt.Finally is { } fin) {
                w.WriteLine($"{indent}    defer {{");
                TranspileBlock(fin.Block, model, indent + "        ", w);
                w.WriteLine($"{indent}    }}");
            }
            TranspileBlock(block, model, indent + "    ", w);
            foreach (var catchClause in catches) {
                w.Write($"{indent}}} catch ");
                if (catchClause.Declaration is { } decl) {
                    w.Write (GetSwiftTypeName(decl.Type, model));
                }                
                if (catchClause.Filter is { } filter) {
                    var cond = TranspileExpression(filter.FilterExpression, model);
                    w.WriteLine($"where {cond}");
                }
                w.WriteLine($" {{");
                TranspileBlock(catchClause.Block, model, indent + "    ", w);
                w.WriteLine($"{indent}}}");
            }
        }
        else {
            w.WriteLine($"{indent}{{");
            if (stmt.Finally is { } fin) {
                w.WriteLine($"{indent}    defer {{");
                TranspileBlock(fin.Block, model, indent + "        ", w);
                w.WriteLine($"{indent}    }}");
            }
            TranspileBlock(block, model, indent + "    ", w);
            w.WriteLine($"{indent}}}");
        }
    }

    void TranspileUsingStatement(UsingStatementSyntax stmt, SemanticModel model, string indent, TextWriter w)
    {
        w.WriteLine($"{indent}{{");
        if (stmt.Expression is {} expr) {
            var exprCode = TranspileExpression(expr, model);
            w.WriteLine($"{indent}    let using_ = {exprCode}");
        }
        else if (stmt.Declaration is {} decl) {
            TranspileVariableDeclaration(decl, model, indent + "    ", w);
        }
        else {
            Error($"Unsupported using statement");
        }
        w.WriteLine($"{indent}    defer {{");
        if (stmt.Expression is not null) {
            w.WriteLine($"{indent}        using_.Dispose()");
        }
        else if (stmt.Declaration is not null) {
            foreach (var v in stmt.Declaration.Variables) {
                var name = v.Identifier.ValueText;
                w.WriteLine($"{indent}        {name}.Dispose()");
            }
        }
        w.WriteLine($"{indent}    }}");
        TranspileStatement(stmt.Statement, model, indent + "    ", w);
        w.WriteLine($"{indent}}}");
    }

    void TranspileVariableDeclaration(VariableDeclarationSyntax decl, SemanticModel model, string indent, TextWriter w)
    {
        var declType = model.GetTypeInfo(decl.Type).Type;
        var declTypeName = GetSwiftTypeName(declType);
        foreach (var v in decl.Variables)
        {
            var vn = v.Identifier.ToString();
            var initCode = v.Initializer is not null ? TranspileExpression(v.Initializer.Value, model) : GetDefaultValue(declType);
            w.WriteLine($"{indent}var {vn}: {declTypeName} = {initCode}");
        }
    }

    void TranspileWhileStatement(WhileStatementSyntax stmt, SemanticModel model, string indent, TextWriter w)
    {
        var cond = TranspileExpression(stmt.Condition, model);
        w.WriteLine($"{indent}while {cond} {{");
        TranspileStatement(stmt.Statement, model, indent + "    ", w);
        w.WriteLine($"{indent}}}");
    }
}
