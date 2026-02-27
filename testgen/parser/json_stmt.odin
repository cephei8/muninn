package testgen

import "core:encoding/json"
import "core:odin/ast"

stmt_to_json :: proc(s: ^ast.Stmt) -> json.Value {
	if s == nil {
		return nil
	}

	#partial switch n in s.derived_stmt {
	case ^ast.Value_Decl:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.decl_base.stmt_base)
		m["node"] = json.String("ValueDecl")
		m["pos"] = p
		m["end"] = end
		m["attributes"] = attributes_to_json(n.attributes)
		m["names"] = exprs_to_json(n.names)
		m["type"] = nullable_expr(n.type)
		m["values"] = exprs_to_json(n.values)
		m["is_mutable"] = json.Boolean(n.is_mutable)
		return m

	case ^ast.Import_Decl:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.decl_base.stmt_base)
		m["node"] = json.String("ImportDecl")
		m["pos"] = p
		m["end"] = end
		if n.name.text != "" {
			m["alias"] = json.String(clone_string(n.name.text))
		} else {
			m["alias"] = nil
		}
		path := n.relpath.text
		if len(path) >= 2 && path[0] == '"' && path[len(path)-1] == '"' {
			path = path[1:len(path)-1]
		}
		m["path"] = json.String(clone_string(path))
		return m

	case ^ast.Foreign_Import_Decl:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.decl_base.stmt_base)
		m["node"] = json.String("ForeignImportDecl")
		m["pos"] = p
		m["end"] = end
		if n.name != nil {
			m["name"] = json.String(clone_string(n.name.name))
		} else {
			m["name"] = json.String("")
		}
		paths_arr := make(json.Array, 0, len(n.fullpaths))
		for fp in n.fullpaths {
			lit, ok := fp.derived_expr.(^ast.Basic_Lit)
			if ok {
				txt := lit.tok.text
				if len(txt) >= 2 && txt[0] == '"' && txt[len(txt)-1] == '"' {
					txt = txt[1:len(txt)-1]
				}
				append(&paths_arr, json.Value(json.String(clone_string(txt))))
			}
		}
		m["paths"] = paths_arr
		return m

	case ^ast.Foreign_Block_Decl:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.decl_base.stmt_base)
		m["node"] = json.String("ForeignBlockDecl")
		m["pos"] = p
		m["end"] = end
		m["library"] = nullable_expr(n.foreign_library)
		m["body"] = nullable_stmt(n.body)
		return m

	case ^ast.Package_Decl:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.decl_base.stmt_base)
		m["node"] = json.String("PackageDecl")
		m["pos"] = p
		m["end"] = end
		m["name"] = json.String(clone_string(n.name))
		return m

	case ^ast.Expr_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("ExprStmt")
		m["pos"] = p
		m["end"] = end
		m["expr"] = expr_to_json(n.expr)
		return m

	case ^ast.Assign_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("AssignStmt")
		m["pos"] = p
		m["end"] = end
		m["lhs"] = exprs_to_json(n.lhs)
		m["op"] = json.String(token_to_assignop(n.op.kind))
		m["rhs"] = exprs_to_json(n.rhs)
		return m

	case ^ast.Block_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("BlockStmt")
		m["pos"] = p
		m["end"] = end
		m["label"] = nullable_expr(n.label)
		m["stmts"] = stmts_to_json(n.stmts)
		return m

	case ^ast.If_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("IfStmt")
		m["pos"] = p
		m["end"] = end
		m["init"] = nullable_stmt(n.init)
		m["cond"] = expr_to_json(n.cond)
		m["body"] = stmt_to_json(n.body)
		m["else"] = nullable_stmt(n.else_stmt)
		return m

	case ^ast.When_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("WhenStmt")
		m["pos"] = p
		m["end"] = end
		m["cond"] = expr_to_json(n.cond)
		m["body"] = stmt_to_json(n.body)
		m["else"] = nullable_stmt(n.else_stmt)
		return m

	case ^ast.For_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("ForStmt")
		m["pos"] = p
		m["end"] = end
		m["init"] = nullable_stmt(n.init)
		m["cond"] = nullable_expr(n.cond)
		m["post"] = nullable_stmt(n.post)
		m["body"] = stmt_to_json(n.body)
		return m

	case ^ast.Range_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("RangeStmt")
		m["pos"] = p
		m["end"] = end
		m["vals"] = exprs_to_json(n.vals)
		m["range"] = expr_to_json(n.expr)
		m["body"] = stmt_to_json(n.body)
		m["reverse"] = json.Boolean(n.reverse)
		return m

	case ^ast.Switch_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("SwitchStmt")
		m["pos"] = p
		m["end"] = end
		m["init"] = nullable_stmt(n.init)
		m["tag"] = nullable_expr(n.cond)
		m["body"] = stmt_to_json(n.body)
		m["partial"] = json.Boolean(n.partial)
		return m

	case ^ast.Type_Switch_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("TypeSwitchStmt")
		m["pos"] = p
		m["end"] = end
		m["init"] = nullable_stmt(n.tag)
		m["tag"] = expr_to_json(n.expr)
		m["body"] = stmt_to_json(n.body)
		m["partial"] = json.Boolean(n.partial)
		return m

	case ^ast.Case_Clause:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("CaseClause")
		m["pos"] = p
		m["end"] = end
		m["exprs"] = exprs_to_json(n.list)
		m["stmts"] = stmts_to_json(n.body)
		return m

	case ^ast.Return_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("ReturnStmt")
		m["pos"] = p
		m["end"] = end
		m["values"] = exprs_to_json(n.results)
		return m

	case ^ast.Defer_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("DeferStmt")
		m["pos"] = p
		m["end"] = end
		m["stmt"] = stmt_to_json(n.stmt)
		return m

	case ^ast.Branch_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("BranchStmt")
		m["pos"] = p
		m["end"] = end
		m["kind"] = json.String(branch_kind_to_string(n.tok.kind))
		if n.label != nil {
			m["label"] = expr_to_json(cast(^ast.Expr)n.label)
		} else {
			m["label"] = nil
		}
		return m

	case ^ast.Using_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("UsingStmt")
		m["pos"] = p
		m["end"] = end
		m["exprs"] = exprs_to_json(n.list)
		return m

	case ^ast.Bad_Stmt:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("BadStmt")
		m["pos"] = p
		m["end"] = end
		return m

	case ^ast.Bad_Decl:
		m := make(json.Object)
		p, end := node_pos_end(&n.node.decl_base.stmt_base)
		m["node"] = json.String("BadStmt")
		m["pos"] = p
		m["end"] = end
		return m

	case ^ast.Unroll_Range_Stmt:
		m := make(json.Object)
		_, end := node_pos_end(&n.node.stmt_base)
		m["node"] = json.String("RangeStmt")
		m["pos"] = pos_to_json(n.for_pos)
		m["end"] = end
		vals := make(json.Array, 0, 2)
		if n.val0 != nil {
			append(&vals, expr_to_json(n.val0))
		}
		if n.val1 != nil {
			append(&vals, expr_to_json(n.val1))
		}
		m["vals"] = vals
		m["range"] = expr_to_json(n.expr)
		m["body"] = stmt_to_json(n.body)
		m["reverse"] = json.Boolean(false)
		return m

	case ^ast.Empty_Stmt:
		return nil

	case:
		m := make(json.Object)
		m["node"] = json.String("UnknownStmt")
		return m
	}
}
